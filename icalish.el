(require 'calfw)
(require 'icalendar)

;; icalendar--convert-all-timezones ical-list)
;; icalendar--all-events
;; from cfw:ical-get-data

;; from calfw.el
(defstruct mycal:event
  title       ; event title [string]
  start-date  ; start date of the event [cfw:date]
  start-time  ; start time of the event (optional)
  end-date    ; end date of the event [cfw:date] (optional)
  end-time    ; end of the event (optional)
  geo         ; lon/lat of this event
  created     ; date/time of creation
  last-modified                ; when was this event last modified
  recurrence-id                ; XXX ???
  rrule                        ; recurrence rule of this event (if any)
  description ; event description [string] (optional)
  location    ; location [strting] (optional)
  source      ; [internal] source of the event
  uid         ; unique identifier for this event (?)
  url         ; URL associated with this event
  )


(defvar mycal:ignored_ics_tags '( acknowledged action attach
                                  attendee begin calscale class
                                  end method organizer priority
                                  prodid rdate related-to status
                                  transp trigger tzid tzname
                                  tzoffsetfrom tzoffsetto version
                                  x-apple-calendar-color
                                  x-apple-default-alarm
                                  x-apple-structured-location
                                  x-apple-travel-advisory-behavior
                                  x-apple-travel-start
                                  x-wr-alarmuid x-wr-calname
                                  x-wr-timezone))

(defvar mycal:used_ics_tags '( created description dtend dtstamp
                               dtstart geo last-modified
                               location recurrence-id rrule
                               summary uid url))

(defun mycal:lexiless-p (l1 l2)
  "is L1 less than L2, comparing element by element"
  (or
   (and (not l1) (not l2))
   (let* ((one (car l1))
          (two (car l2))
          (test1 (if (not one) 0 one))
          (test2 (if (not two) 0 two)))
     (if (< test1 test2)
         t
       (and (= test1 test2)
            (mycal:lexiless-p (cdr l1) (cdr l2)))))))


(defun mycal:start-date-time-less-p (ev1 ev2)
  "does EV1 start before (or at same time) as EV2?"
  (let ((date1 (reverse (mycal:event-start-date ev1)))
        (date2 (reverse (mycal:event-start-date ev2)))
        (time1 (mycal:event-start-time ev1))
        (time2 (mycal:event-start-time ev2)))
    (mycal:lexiless-p (append date1 time1)
                      (append date2 time2))))

;; from (cfw:ical-event-get-dates)
(defun mycal:ical--get-date (event tag)
  "find a TAG in EVENT, and return date and time of it"
  (let*
      ((tval (icalendar--get-event-property event tag))
       (tzone (icalendar--find-time-zone
               (icalendar--get-event-property-attributes event tag) zone-map))
       (tdecode (icalendar--decode-isodatetime tval nil tzone))
       (tdate (cfw:decode-to-calendar tdecode))
       (ttime (cfw:time (nth 2 tdecode) (nth 1 tdecode))))
    (list tdate ttime)))
       
;; cribbed from calfw-ical.el
(defun mycal:ical-convert-event (event)
    (destructuring-bind (dtag date start end) (cfw:ical-event-get-dates event)
    (make-mycal:event
     :start-date  date
     :start-time  start
     :end-date    (when (equal dtag 'period) end)
     :end-time    (when (equal dtag 'time)   end)
     :created     (mycal:ical--get-date event 'CREATED)
     :last-modified (mycal:ical--get-date event 'LAST-MODIFIED)
     :geo         (icalendar--get-event-property event 'GEO)   
     :title       (cfw:ical-sanitize-string
                   (icalendar--get-event-property event 'SUMMARY))
     :location    (cfw:ical-sanitize-string
                   (icalendar--get-event-property event 'LOCATION))
     :description (cfw:ical-sanitize-string
                   (icalendar--get-event-property event 'DESCRIPTION))
     :recurrence-id (icalendar--get-event-property event 'RECURRENCE-ID)
     :rrule       (icalendar--get-event-property event 'RRULE)
     :uid         (icalendar--get-event-property event 'UID)
     :url         (icalendar--get-event-property event 'URL))))

;; cribbed from calfw-ical.el
(defun mycal:ical-convert-ical-to-mycal (ical-list)
  (loop with zone-map = (icalendar--convert-all-timezones ical-list)
        for e in (icalendar--all-events ical-list)
        for event = (mycal:ical-convert-event e)
        if event
        if (mycal:event-end-date event)
        collect event into periods
        else
        collect event into contents
        else do
        (progn
          (message "Ignoring event \"%s\"" e)
          (message "Cannot handle this event, tag: %s" e))
        finally (return `((periods ,periods) ,@contents))))

;; cribbed from calfw-ical.el
(defun mycal:ical-get-data (url)
  (let ((data (assoc url mycal:ical-data-cache)))
    (unless data
      (setq data (let ((cal-list
                        (cfw:ical-with-buffer url
                          (cfw:ical-normalize-buffer)
                          (mycal:ical-convert-ical-to-mycal
                           (icalendar--read-element nil nil)))))
                   (cons url cal-list)))
      (push data mycal:ical-data-cache))
    (cdr data)))

(defun mycal:open-ical-calendar (url)
  (let* ((unsorted (copy-sequence (mycal:ical-get-data url)))
         (sorted (sort (cdr unsorted) 'mycal:start-date-time-less-p)))
    sorted))
