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

(defun mycal:lexiless (l1 l2)
  "is L1 less than L2, comparing element by element"
  (if (< (car l1) (car l2))
      t
    (and (= (car l1) (car l2))
         (mycal:lexiless (cdr l1) (cdr l2)))))

(defun mycal:compare-date (date1 date2)
  (let ((y1 (nth 2 date1))
        (y2 (nth 2 date2))
        (m1 (nth 1 date1))
        (m2 (nth 1 date2))
        (d1 (nth 0 date1))
        (d2 (nth 0 date2)))
    (or
     (< y1 y2)
     (and (= y1 y2)
          (or
           (< m1 m2)
           (and (= m1 m2)
                (< d1 d2)))))))

(defun mycal:compare-time (time1 time2)
  (let ((h1 (nth 0 time1))
        (h2 (nth 0 time2))
        (m1 (nth 1 time1))
        (m2 (nth 1 time2)))
    

(defun mycal:compare-start-date-time (ev1 ev2)
  "does EV1 start before (or at same time) as EV2?"
  (let ((date1 (mycal:event-start-date ev1))
        (date2 (mycal:event-start-date ev2)))
    (if (mycal:compare-dates date1 date2)
        t
      (if (mycal:compare-dates date2 date1)
          nil
        (let ((time1 (mycal:event-start-time ev1))
              (time2 (mycal:event-start-time ev2)))
          (not (mycal:compare-times date2 date1)))))))

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
     :rrule       (icalendar--get-event-property event 'RRULE))))

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
