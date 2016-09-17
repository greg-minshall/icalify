(require 'calfw)
(require 'icalendar)

;; icalendar--convert-all-timezones ical-list)
;; icalendar--all-events
;; from cfw:ical-get-data


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
                               dtstart exdate geo last-modified
                               location recurrence-id rrule
                               sequence summary uid url))

(defun mycal:ical-convert-ical-to-mycal (ical-list)
  (loop with zone-map = (icalendar--convert-all-timezones ical-list)
        for e in (icalendar--all-events ical-list)
        for event = (mycal:ical-convert-event e)
        if event
        if (cfw:event-end-date event)
        collect event into periods
        else
        collect event into contents
        else do
        (progn
          (message "Ignoring event \"%s\"" e)
          (message "Cannot handle this event, tag: %s" e))
        finally (return `((periods ,periods) ,@contents))))

(defun mycal:ical-get-data (url)
  (let ((data (assoc url cfw:ical-data-cache)))
    (unless data
      (setq data (let ((cal-list
                        (cfw:ical-with-buffer url
                          (cfw:ical-normalize-buffer)
                          (mycal:ical-convert-ical-to-mycal
                           (icalendar--read-element nil nil)))))
                   (cons url cal-list)))
      (push data cfw:ical-data-cache))
    (cdr data)))
