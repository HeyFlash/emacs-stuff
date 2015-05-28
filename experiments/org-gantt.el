;;; org-gantt-experiment.el --- This is org-gantt, which allows to create gantt charts from deadlines, schedules, efforts and clock values.
;;| start | end | effort | dependency |
;;|       |     |        |            |
;;
;;| start end | start effort  | start dependency | end effort      | end dependency  | effort dependency       |
;;| use       | calculate end | ERROR            | calculate start | calculate start | calculate start and end |
;;
;;| start end effort | start end dependency | start effort dependency | end effort dependency |
;;| ERROR            | ERROR                | ERROR                   | ERROR                 |
;;
;;| start end effort dependency |
;;| ERROR                       |


;;; Commentary:
;; 

;;; Code:

(defgroup org-gantt nil "Customization of org-gantt.")

(defcustom org-gantt-default-hours-per-day 8
  "The default hours in a workday.  
Use :hours-per-day to overwrite this value for individual gantt charts."
  :type '(integer)
  :group 'org-gantt)

(defcustom org-gantt-default-weekend-style "{black}"
  "The default style for the weekend lines. 
Use :weekend-style to overwrite this value for individual gantt charts."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-default-workday-style "{dashed}"
  "The default style for the workday lines.  
Use :workday-style to overwrite this value for individual gantt charts."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-default-title-calendar "year, month=name, day"
  "The default style for the title calendar. 
Use :title-calendar to overwrite this value for individual gantt charts."
  :type '(string)
  :group 'org-gantt)

(defconst org-gantt-start-prop :startdate
  "What is used as the start property in the constructed property list.")

(defconst org-gantt-end-prop :enddate
  "What is used as the end property in the constructed property list.")

(defconst org-gantt-effort-prop :effort
  "What is used as the effort property in the constructed property list.")

(defconst org-gantt-clocksum-prop :clocksum
  "What is used as the effort property in the constructed property list.")

(defconst org-gantt-progress-prop :progress
  "What is used as the progress property in the constructed property list.")

(defun org-gantt-has-effort (aplist)
  "Return non-nil if an alist or plist contains effort.
Argument APLIST an alist or plist."
(or (plist-get aplist ':effort)
    (assoc "effort" aplist)))

(defun org-gantt-has-startdate (aplist)
  "Returns non-nil if an alist or plist contains a startdate.
Argument APLIST an alist or plist."
(or (plist-get aplist ':scheduled)
    (assoc "SCHEDULED" aplist)))

(defun org-gantt-has-enddate (aplist)
  "Return non-nil if an alist or plist contains an enddate.
Argument APLIST an alist or plist."
  (or (plist-get aplist ':deadline)
      (assoc "DEADLINE" aplist)))

(defvar org-gant-hours-per-day-gv nil
  "Global variable for local hours-per-day.")

(defun org-gantt-hours-per-day ()
  "Get the hours per day."
  org-gantt-hours-per-day-gv)

(defun org-gantt-hours-per-day-time ()
  "Get hours per day as a time value."
  (seconds-to-time (* 3600 (org-gantt-hours-per-day))))

(defun org-gantt-get-planning-time (element timestamp-type)
  "Get the time belonging to a first-order headline of the given ELEMENT.
TIMESTAMP-TYPE is either :scheduled or :deadline.
If it is :deadline, hours-per-day is added to it."
  (let* ((timestamp
	  (org-element-map
	   element '(planning headline)
	   (lambda (subelement) (org-element-property timestamp-type subelement))
	   nil t 'headline))
	 (time (org-gantt-timestamp-to-time timestamp))
	 (dt (decode-time time))
	 (hours (nth 2 dt))
	 (minutes (nth 1 dt)))
    (if (and (equal timestamp-type :deadline)
	     (= 0 hours)
	     (= 0 minutes))
	(time-add time (org-gantt-hours-per-day-time))
      time)))

(defun org-gantt-get-subheadlines (element)
  (org-element-map element 'headline (lambda (subelement) subelement)
		   nil nil 'headline))

(defun org-gantt-tp-compare (tp1 tp2 key compare-function)
  "Time property compare, works with nil."
  (and (org-element-property key tp1)
       (or (not (org-element-property key tp2))
	   (funcall compare-function (org-element-property key tp1) (org-element-property key tp2)))))

(defun org-gantt-timestamp-smaller (ts1 ts2)
  "Returns true iff timestamp ts1 is before ts2"
  (and ts1
       (or (not ts2)
           ;;I'm sure that's not how it should be done...
           (if (org-gantt-tp-compare ts1 ts2 ':year-start #'=)
               (if (org-gantt-tp-compare ts1 ts2 ':month-start #'=)
                   (if (org-gantt-tp-compare ts1 ts2 ':day-start #'=)
                       (if (org-gantt-tp-compare ts1 ts2 ':hour-start #'=)
                           (if (org-gantt-tp-compare ts1 ts2 ':minute-start #'=)
                               (org-gantt-tp-compare ts1 ts2 ':minute-start #'<))
                         (org-gantt-tp-compare ts1 ts2 ':hour-start #'<))
                     (org-gantt-tp-compare ts1 ts2 ':day-start #'<))
                 (org-gantt-tp-compare ts1 ts2 ':month-start #'<))
             (org-gantt-tp-compare ts1 ts2 ':year-start #'<)))))

(defun org-gantt-timestamp-larger (ts1 ts2)
  "Returns true iff not timestamp ts1 is before ts2"
  (not (org-gantt-timestamp-smaller ts1 ts2)))

(defun org-gantt-time-less-p (t1 t2)
  "time-less-p working with nil."
  (and t1
       (or (not t2)
           (time-less-p t1 t2))))

(defun org-gantt-subheadline-extreme (element comparator time-getter subheadline-getter)
  "Returns the smallest/largest timestamp of the subheadlines
of element according to comparator.
time-getter is the recursive function that needs to be called if
the subheadlines have no timestamp."
  (and
   element
   (let ((subheadlines (funcall subheadline-getter element)))
     (funcall
      time-getter
      (car
       (sort
	subheadlines
	(lambda (hl1 hl2)
	  (funcall comparator
		   (funcall time-getter hl1)
		   (funcall time-getter hl2)))))))))

(defun org-gantt-get-start-time (element)
""
(or
 (org-gantt-get-planning-time element ':scheduled)
 (org-gantt-subheadline-extreme
  (cdr element)
  #'org-gantt-time-less-p
  #'org-gantt-get-start-time
  #'org-gantt-get-subheadlines)))

(defun org-gantt-get-end-time (element)
""
(or
 (org-gantt-get-planning-time element ':deadline)
 (org-gantt-subheadline-extreme
  (cdr element)
  (lambda (ts1 ts2)
    (not (org-gantt-time-less-p ts1 ts2)))
  #'org-gantt-get-end-time
  #'org-gantt-get-subheadlines)))

(defun org-gantt-sum-times (e1 e2 day-adder)
  "Sums e1 and e2, taking into account hours-per-workday."
  (let* ((e1s (round (time-to-seconds e1)))
         (e2s (round (time-to-seconds e2)))
         (e1h (/ 3600 e1s))
         (e2h (/ 3600 e2s))
         (e1d (/ (org-gantt-hours-per-day) e1h))
         (e2d (/ (org-gantt-hours-per-day) e2h))
         (ad (+ e1d e2d))
         (e1time (decode-time e1))
         (e2time (decode-time e2))
         (e1min (nth 1 e1time))
         (e2min (nth 1 e2time))
         (e1hour (nth 2 e1time))
         (e2hour (nth 2 e2time))
         (minsum (+ e1min e2min))
         (hoursum (+ e1hour e2hour))
         (totalmin (% minsum 60))
         (totalhour (+ hoursum (/ minsum 60)))
         (e1trunc (encode-time 0 0 0 (nth 3 e1time) (nth 4 e1time) (nth 5 e1time)))
         (e2trunc (encode-time 0 0 0 (nth 3 e2time) (nth 4 e2time) (nth 5 e2time)))
         (adddays (/ totalhour (org-gantt-hours-per-day)))
         (addtime (seconds-to-time
                   (+ (* 60 totalmin)
                      (* 3600 totalhour))))
         (truncsum (time-add e1trunc e2trunc))
         (withtime (time-add truncsum addtime)))
    (funcall day-adder withtime adddays)))

(defun org-gantt-subheadlines-effort (element effort-getter element-org-gantt-effort-prop)
  "Returns the sum of the efforts of the subheadlines
of element according to comparator.
effort-getter is the recursive function that needs to be called if
the subheadlines have no effort."
  (and
   element
   (let ((subheadlines (org-gantt-get-subheadlines element))
         (time-sum (seconds-to-time 0)))
     (dolist (sh subheadlines (if (= 0 (apply '+ time-sum)) nil time-sum))
       (let ((subtime (funcall effort-getter sh element-org-gantt-effort-prop)))
         (when subtime
           (setq time-sum (time-add time-sum subtime))))))))


(defun org-gantt-get-effort (element element-org-gantt-effort-prop &optional use-subheadlines-effort)
  "Get the effort of the current element.
If use-subheadlines-effort is non-nil and element has no effort,
use sum of the efforts of the subelements."
  (let ((effort-time (org-gantt-effort-to-time (org-element-property element-org-gantt-effort-prop element))))
    (or effort-time
        (and use-subheadlines-effort
             (org-gantt-subheadlines-effort (cdr element) #'org-gantt-get-effort element-org-gantt-effort-prop)))))

(defun org-gantt-create-gantt-info (element)
  "Creates a gantt-info for element.
A gantt-info is a plist containing :name org-gantt-start-prop org-gantt-end-prop org-gantt-effort-prop :subelements"
  (list ':name (org-element-property ':raw-value element)
	':ordered (org-element-property :ORDERED element)
        org-gantt-start-prop (org-gantt-get-start-time element)
        org-gantt-end-prop (org-gantt-get-end-time element)
        org-gantt-effort-prop (org-gantt-get-effort 
                               element :EFFORT
                               ;(make-symbol (concat ":" (upcase org-effort-property)))
                               )
        org-gantt-clocksum-prop (org-gantt-effort-to-time (org-element-property :CLOCKSUM element) 24) ;clocksum is computed automatically with 24 hours per day, therefore we use 24.
;        (org-gantt-get-effort element :CLOCKSUM)
        ':subelements (org-gantt-crawl-headlines (cdr element))))

(defun org-gantt-crawl-headlines (data)
  ""
  (let ((gantt-info-list
	 (org-element-map data 'headline #'org-gantt-create-gantt-info nil nil 'headline)))
    gantt-info-list))

(defun org-gantt-timestamp-to-string (timestamp)
  "should be replaced by org-timestamp-format, but that doesn't seem to work."
  (let ((ts (cadr timestamp)))
    (concat
     (number-to-string (plist-get ts ':year-start))
     "-"
     (number-to-string (plist-get ts ':month-start))
     "-"
     (number-to-string
      
      (if (and (plist-get ts :hour-start)
               (= (plist-get ts :hour-start) 0)
               (plist-get ts :minute-start)
               (= (plist-get ts :minute-start) 0))
          (if (equal (plist-get ts :type) 'forward-computed)
              (- (plist-get ts :day-start) 1)
            (if (equal (plist-get ts :type) 'backward-computed)
                (+ (plist-get ts :day-start) 1)
              (plist-get ts :day-start)))
        (plist-get ts :day-start))))))

(defun org-gantt-timestamp-to-pgf-end (timestamp)
  "Converts a timestamp to a pfg end time string. Specifically uses the day before,
If the current ")

(defun org-gantt-get-extreme-date (data time-getter timestamp-comparer)
  "Get the first or last date (depending on timestamp-comparer)
of all the timestamps in data"
  (let ((timestamps
         (org-element-map data 'headline
           (lambda (headline)
             (funcall time-getter headline)))))
    (car (sort timestamps timestamp-comparer))))


(defun org-gantt-timestamp-to-time (timestamp &optional use-end)
  "Converts a timestamp to an emacs time.
If optional use-end is non-nil use the ...-end values of the timestamp."
  (and timestamp
       (if use-end
           (encode-time 0
                        (or (org-element-property :minute-end timestamp) 0)
                        (or (org-element-property :hour-end timestamp) 0)
                        (org-element-property :day-end timestamp)
                        (org-element-property :month-end timestamp)
                        (org-element-property :year-end timestamp))
         (encode-time 0
                      (or (org-element-property :minute-start timestamp) 0)
                      (or (org-element-property :hour-start timestamp) 0)
                      (org-element-property :day-start timestamp)
                      (org-element-property :month-start timestamp)
                      (org-element-property :year-start timestamp)))))

(defun org-gantt-time-to-timestamp (time &optional type)
  "Converts an emacs time to an org-mode timestamp."
  (let ((dt (decode-time time)))
    (and time
         (list 'timestamp
               (list :type (or type 'computed) :raw-value (format-time-string "<%Y-%m-%d %a>" time)
                     :year-start (nth 5 dt) :month-start (nth 4 dt) :day-start (nth 3 dt)
                     :hour-start (nth 2 dt) :minute-start (nth 1 dt)
                     :year-end (nth 5 dt) :month-end (nth 4 dt) :day-end (nth 3 dt)
                     :hour-end (nth 2 dt) :minute-end (nth 1 dt))))))

(defun org-gantt-substring-if (string from to)
  "Returns substring if string, from and to are non-nil and from < to, otherwise nil"
  (and string from to (< from to) (substring string from to)))

(defun org-gantt-string-to-number (string)
  "string-to-number that returns 0 for nil argument"
  (if string (string-to-number string) 0))

(defun org-gantt-strings-to-time
  (seconds-string minutes-string &optional hours-string
   days-string weeks-string months-string years-string hours-per-day)
  "Convert the given strings to time, taking into account hours-per-day."
  (let* ((ex-hours (+ (org-gantt-string-to-number seconds-string)
                      (* 60 (org-gantt-string-to-number minutes-string))
                      (* 3600 (org-gantt-string-to-number hours-string))))
         (calc-days (/ ex-hours (org-gantt-hours-per-day)))
         (rest-hours (% ex-hours (org-gantt-hours-per-day)))
         (time
          (seconds-to-time
           (+ (org-gantt-string-to-number seconds-string)
              (* 60 (org-gantt-string-to-number minutes-string))
              (* 3600 (org-gantt-string-to-number hours-string))
              (* 3600 (or hours-per-day (org-gantt-hours-per-day)) (org-gantt-string-to-number days-string))
              (* 3600 (or hours-per-day (org-gantt-hours-per-day)) 30 (org-gantt-string-to-number months-string))
              (* 3600 (or hours-per-day (org-gantt-hours-per-day)) 30 12 (org-gantt-string-to-number years-string))))))
    (if (= 0 (apply '+ time))
        nil
      time)))

(defun org-gantt-effort-to-time (effort &optional hours-per-day)
  "Parses an effort timestring and returns it as emacs time representing a time difference.
Optional hours-per-day makes it possible to convert hour estimates into workdays."
  (and effort
       (let* ((years-string (org-gantt-substring-if effort 0 (string-match "y" effort)))
              (msp (if years-string (match-end 0) 0))
              (months-string (org-gantt-substring-if effort msp (string-match "m" effort)))
              (wsp (if months-string (match-end 0) msp))
              (weeks-string (org-gantt-substring-if effort wsp (string-match "w" effort)))
              (dsp (if weeks-string (match-end 0) wsp))
              (days-string (org-gantt-substring-if effort dsp (string-match "d" effort)))
              (hsp (if days-string (match-end 0) dsp))
              (hours-string (org-gantt-substring-if effort hsp (string-match ":" effort)))
              (minsp (if hours-string (match-end 0) hsp))
              (minutes-string (org-gantt-substring-if effort minsp (length effort))))
         (org-gantt-strings-to-time "0"
                                    minutes-string hours-string
                                    days-string weeks-string
                                    months-string years-string
                                    hours-per-day))))

(defun org-gantt-is-workday (time)
  "Returns non-nil, iff time is a workday. Currently does not consider holidays."
  (let ((dow (string-to-number (format-time-string "%w" time))))
    (and (/= dow 6)
         (/= dow 0))))

(defun org-gantt-change-workdays (time ndays change-function)
  "Add or subtract (depending on change-function) ndays workdays to the given time.
E.g. if time is on Friday, ndays is one, the result will be monday.
FIXME: Does not use holidays."
  (cl-assert (>= ndays 0) "trying to add negative days to timestamp.")
  (let ((oneday (days-to-time 1))
        (curtime time))
    (while (/= 0 ndays)
      (setq curtime (funcall change-function curtime oneday))
      (when (org-gantt-is-workday curtime)
        (setq ndays (- ndays 1))))
    curtime))

(defun org-gantt-day-end (time)
  "Get the end of the given workday."
  (let ((dt (decode-time time)))
    (encode-time 0 0 (org-gantt-hours-per-day)
                 (nth 3 dt) (nth 4 dt) (nth 5 dt))))

(defun org-gantt-day-start (time)
  "Get the start of the given workday."
  (let ((dt (decode-time time)))
    (encode-time 0 0 0 (nth 3 dt) (nth 4 dt) (nth 5 dt))))


(defun org-gantt-add-worktime (time change-time)
  "Add change-time to time, taking into account holidays and hours-per-day."
  (let* ((dt (decode-time time))
         (day-end (encode-time 0 0 (org-gantt-hours-per-day)
                               (nth 3 dt) (nth 4 dt) (nth 5 dt)))
         (rest-time (time-subtract day-end time))
         (one-day (days-to-time 1)))
    (if (time-less-p change-time rest-time)
	(time-add time change-time)
      (let*
	  ((next-day-d (decode-time (org-gantt-change-workdays time 1 #'time-add)))
	   (next-day (encode-time 0 0 0 (nth 3 next-day-d)
				  (nth 4 next-day-d) (nth 5 next-day-d)))
	   (rest-change (time-subtract change-time rest-time))
	   (dc (decode-time rest-change))
	   (rest-min (+ (nth 1 dc) (* 60 (nth 2 dc)))))
	(while (> rest-min (* 60 (org-gantt-hours-per-day)))
	  (setq next-day (org-gantt-change-workdays next-day 1 #'time-add))
	  (setq rest-change (time-subtract rest-change (seconds-to-time (* 3600 (org-gantt-hours-per-day)))))
	  (setq dc (decode-time rest-change))
	  (setq rest-min (+ (nth 1 dc) (* 60 (nth 2 dc)))))
	(while (time-less-p one-day rest-change)
	  (setq next-day (org-gantt-change-workdays next-day 1 #'time-add))
	  (setq rest-change (time-subtract rest-change one-day)))
	(time-add next-day rest-change)))))

(defun org-gantt-time-difference (t1 t2)
  (if (time-less-p t1 t2)
      (time-subtract t2 t1)
    (time-subtract t1 t2)))

(defun org-gantt-change-worktime (time change-time time-changer day-start-getter day-end-getter)
  "Add change-time to time, taking into account holidays and hours-per-day."
  (let* ((day-end (funcall day-end-getter time))
         (rest-time (org-gantt-time-difference day-end time))
         (one-day (days-to-time 1)))
;    (message "Change Time: %s" (format-time-string "%Y-%m-%d:%T" change-time))
    (if (time-less-p change-time rest-time)
	(funcall time-changer time change-time)
      (let*
	  ((next-day (funcall day-start-getter (org-gantt-change-workdays time 1 time-changer)))
	   (rest-change (time-subtract change-time rest-time))
	   (rest-sec (round (time-to-seconds rest-change))))
	(while (> rest-sec (* 3600 (org-gantt-hours-per-day)))
	  (setq next-day (org-gantt-change-workdays next-day 1 time-changer))
	  (setq rest-change (time-subtract rest-change (seconds-to-time (* 3600 (org-gantt-hours-per-day)))))
	  (setq rest-sec (round (time-to-seconds rest-change))))
	(while (time-less-p one-day rest-change)
	  (setq next-day (org-gantt-change-workdays next-day 1 time-changer))
	  (setq rest-change (time-subtract rest-change one-day)))
	(funcall time-changer next-day rest-change)))))

(defun org-gantt-get-next-time (endtime)
  "Get the time where the next bar should start."
  (let* ((dt (decode-time endtime))
	 (hours (nth 3 dt))
	 (minutes (nth 2 dt)))
    (if (and (= (org-gantt-hours-per-day) hours)
	     (= 0 minutes))
	(org-gantt-change-worktime
         endtime (encode-time (* 3600 (- 24 (org-gantt-hours-per-day))))
         #'time-add
         #'org-gantt-day-start #'org-gantt-day-end)
      endtime)))


(defun org-gantt-get-prev-time (starttime)
  "Get the time where the previous bar should end."
  (let* ((dt (decode-time starttime))
	 (hours (nth 3 dt))
	 (minutes (nth 2 dt)))
    (if (and (= (org-gantt-hours-per-day) hours)
	     (= 0 minutes))
        (org-gantt-change-worktime
         starttime (seconds-to-time (* 3600 (- 24 (org-gantt-hours-per-day))))
         #'time-subtract
         #'org-gantt-day-end #'org-gantt-day-start)
      starttime)))


(defun org-gantt-propagate-order-timestamps (headline-list &optional is-ordered parent-start parent-end)
  "Propagate the times of headlines that are ordered according to their superheadline.
Recursively apply to subheadlines."
  (let ((next-start (or (plist-get (car headline-list) org-gantt-start-prop) parent-start))
        (listitem headline-list)
        (headline nil)
        (replacement nil)
        (newlist nil))
    (while listitem
      (setq headline (car listitem))
      (setq replacement headline)
      (when is-ordered
        (setq replacement
              (plist-put replacement org-gantt-start-prop
                         (or (plist-get replacement org-gantt-start-prop) next-start)))
        (setq next-start (org-gantt-get-next-time (plist-get replacement org-gantt-end-prop)))
        (setq replacement
              (plist-put replacement org-gantt-end-prop
                         (or (plist-get replacement org-gantt-end-prop)
                             (if (cdr listitem)
                                 (org-gantt-get-prev-time
                                  (plist-get (cadr listitem) org-gantt-start-prop))
                               parent-end)))))
      (when (plist-get replacement :subelements)
        (setq replacement
              (plist-put replacement :subelements
                         (org-gantt-propagate-order-timestamps
                          (plist-get replacement :subelements)
                          (or is-ordered (plist-get replacement :ordered))
                          (plist-get replacement org-gantt-start-prop)
                          (plist-get replacement org-gantt-end-prop)))))
      (setq newlist (append newlist (list replacement)))
      (setq listitem (cdr listitem)))
    newlist))

(defun org-gantt-calculate-ds-from-effort (headline-list)
  "Calculates deadline or schedule from effort.
If a deadline or schedule conflicts with the effort, keep value and warn."
  (let ((newlist nil))
    (dolist (headline headline-list newlist)
      (let ((replacement headline)
            (start (plist-get headline org-gantt-start-prop))
            (end (plist-get headline org-gantt-end-prop))
            (effort (plist-get headline org-gantt-effort-prop)))
        (cond ((and start end effort)
               effort) ;;FIXME: Calculate if start, end, effort conflict and warn.
              ((and start effort)
               (setq replacement
                     (plist-put replacement org-gantt-end-prop
				(org-gantt-change-worktime
                                 start effort
                                 #'time-add
                                 #'org-gantt-day-start
                                 #'org-gantt-day-end))))
              ((and effort end)
               (setq replacement
                     (plist-put replacement org-gantt-start-prop
				(org-gantt-change-worktime
                                 end effort
                                 #'time-subtract
                                 #'org-gantt-day-end
                                 #'org-gantt-day-start)))))
        (when (plist-get replacement :subelements)
          (setq replacement
                (plist-put replacement :subelements
                           (org-gantt-calculate-ds-from-effort
                            (plist-get replacement :subelements)))))
        (setq newlist (append newlist (list replacement)))))))

(defun org-gantt-first-subheadline-start (headline)
  "Gets the start time of the first subelement of headline (or its subelement)."
  (and headline
       (or (plist-get (car (plist-get headline :subelements)) org-gantt-start-prop)
           (org-gantt-get-subheadline-start (car (plist-get headline :subelements)) t))))

(defun org-gantt-last-subheadline-end (headline)
  "Gets the end time of the last subelement of headline (or its subelement)."
  (and headline
       (or (plist-get (car (last (plist-get headline :subelements))) org-gantt-end-prop)
           (org-gantt-get-subheadline-end (car (last (plist-get headline :subelements))) t))))


(defun org-gantt-get-subheadline-start (headline ordered)
  ""
  (or (plist-get headline org-gantt-start-prop)
      (if ordered
          (org-gantt-first-subheadline-start headline)
        (org-gantt-subheadline-extreme
         headline
         #'org-gantt-time-less-p
         (lambda (hl) (org-gantt-get-subheadline-start hl ordered))
         (lambda (hl) (org-gantt-propagate-ds-up (plist-get hl :subelements) ordered))))))

(defun org-gantt-get-subheadline-end (headline ordered)
  ""
  (or (plist-get headline org-gantt-end-prop)
      (if ordered
          (org-gantt-last-subheadline-end headline)
        (org-gantt-subheadline-extreme
         headline
         (lambda (t1 t2)
           (not (org-gantt-time-less-p t1 t2)))
         (lambda (hl) (org-gantt-get-subheadline-end hl ordered))
         (lambda (hl) (org-gantt-propagate-ds-up (plist-get hl :subelements) ordered))))))


(defun org-gantt-get-subheadline-effort-sum (headline)
  ""
  (or (plist-get headline org-gantt-effort-prop)
      (let ((subelements (plist-get headline :subelements))
            (effort-sum (seconds-to-time 0)))
;        (org-gantt-propagate-effort-up subelements)
        (dolist (ch subelements effort-sum)
          (setq effort-sum 
                (time-add effort-sum  
                          (org-gantt-get-subheadline-effort-sum ch)))))))

(defun org-gantt-get-subheadline-progress-summation (headline calc-progress &optional prioritize-subsums)
  ""
  (let ((subelements (plist-get headline :subelements))
        (progress (plist-get headline org-gantt-progress-prop))
        (progress-sum nil)
        (count 0))
    (message "Subelements: %s" subelements)
    (or (and (not prioritize-subsums)
             (equal calc-progress 'use-larger-100) 
             progress)
        (and (not prioritize-subsums)
             progress
             (message "USING MINIMUM of (100, %s)" progress)
             (min 100 progress))
                                        ;        (org-gantt-propagate-summation-up subelements)
        (dolist (ch subelements (and progress-sum count (round (/ progress-sum count))))
          (let ((subsum (org-gantt-get-subheadline-progress-summation ch calc-progress prioritize-subsums))
                (subeffort (time-to-seconds (plist-get ch :effort))))
            (setq count (+ count subeffort))
            (message "ps: %s, ss: %s" progress-sum subsum)
            (setq progress-sum 
                  (cond 
                   ((and progress-sum subsum)
                    (+ progress-sum (* subeffort subsum)))
                   (progress-sum progress-sum)
                   (subsum (* subeffort subsum))
                   (t nil)))))
        (and (equal calc-progress 'use-larger-100) 
             progress)
        (and  progress
             (min 100 progress)))))

(defun org-gantt-propagate-ds-up (headline-list &optional ordered)
  "Propagates start and end time from subelements."
  (let ((newlist nil))
    (dolist (headline headline-list newlist)
      (let ((replacement headline)
            (cur-ordered (or ordered (plist-get headline :ordered)))
            (start (plist-get headline org-gantt-start-prop))
            (end (plist-get headline org-gantt-end-prop)))
        (setq replacement
              (plist-put replacement org-gantt-start-prop
                         (org-gantt-get-subheadline-start replacement cur-ordered)))
        (setq replacement
              (plist-put replacement org-gantt-end-prop
                         (org-gantt-get-subheadline-end replacement cur-ordered)))
        (setq newlist (append newlist (list replacement)))))))

(defun org-gantt-propagate-summation-up (headline-list property subsum-getter &optional prioritize-subsums)
  "Propagates summed efforts from subelements."
  (let ((newlist nil))
    (dolist (headline headline-list newlist)
      (let ((replacement headline)
            (effort (plist-get headline property)))
        (when (or prioritize-subsums (not effort))
          (setq replacement
                (plist-put replacement property
                           (funcall subsum-getter replacement))))
        (setq newlist (append newlist (list replacement)))))))

(defun org-gantt-compute-progress (headline-list)
  "Computes the progress (if possible) for the headlines in headlines-list.
Is recursively applied to subelements."
  (let ((newlist nil))
    (dolist (headline headline-list newlist)
      (let ((replacement headline)
            (effort (plist-get headline org-gantt-effort-prop))
            (clocksum (plist-get headline org-gantt-clocksum-prop))
            (subelements (plist-get headline :subelements)))
        (when (and effort clocksum)
          (setq replacement
                (plist-put replacement org-gantt-progress-prop
                           (org-gantt-get-completion-percent effort clocksum))))
        (setq replacement 
              (plist-put replacement :subelements
                         (org-gantt-compute-progress subelements)))
        (setq newlist (append newlist (list replacement)))))))

(defun org-gantt-downcast-endtime (endtime)
  (let* ((dt (decode-time endtime))
         (hours (nth 2 dt))
         (minutes (nth 1 dt)))
    (if (and (= 0 hours)
             (= 0 minutes))
        (time-add (org-gantt-change-workdays endtime 1 #'time-subtract)
                  (org-gantt-hours-per-day-time))
      endtime)))

(defun org-gantt-upcast-starttime (starttime)
  (let* ((dt (decode-time starttime))
         (hours (nth 2 dt))
         (minutes (nth 1 dt)))
    (if (and (= 0 minutes)
             (= (org-gantt-hours-per-day) hours))
        (time-subtract (org-gantt-change-workdays starttime 1 #'time-add)
                       (org-gantt-hours-per-day-time))
      starttime)))

(defun org-gantt-get-day-ratio (time)
  "Returns the ratio of a workday that
is in the hour-minute part of time."
  (let* ((dt (decode-time time))
         (hours (nth 2 dt))
         (minutes (nth 1  dt))
         (minsum (+ minutes (* 60 hours))))
    (/ (float minsum) (* 60 (org-gantt-hours-per-day)))))

(defun org-gantt-get-completion-percent (effort clocksum)
  "Returns the percentage of completion of effort as measured by clocksum."
  (if (and clocksum effort)
      (let ((css (time-to-seconds clocksum))
            (es (time-to-seconds effort)))
        (if (> es 0) (round (* 100  (/ css es)))  0))
    0))

(defun org-gantt-info-to-pgfgantt (gi &optional show-progress prefix ordered linked)
  "Creates a pgfgantt string from gantt-info gi"
  (let ((subelements (plist-get gi :subelements))
        (start (plist-get gi org-gantt-start-prop))
        (end (plist-get gi org-gantt-end-prop))
        (up-start (org-gantt-upcast-starttime (plist-get gi org-gantt-start-prop)))
        (down-end (org-gantt-downcast-endtime (plist-get gi org-gantt-end-prop)))
        (effort (plist-get gi org-gantt-effort-prop))
        (clocksum (plist-get gi org-gantt-clocksum-prop))
        (progress (plist-get gi org-gantt-progress-prop)))
    (concat
     prefix
     (if subelements
         (if linked "\\ganttlinkedgroup" "\\ganttgroup")
       (if linked "\\ganttlinkedbar" "\\ganttbar"))
     "["
     (if subelements "group left shift=" "bar left shift=")
     (number-to-string (org-gantt-get-day-ratio up-start))
     (if subelements ", group right shift=" ", bar right shift=")
     (number-to-string (if (>  (org-gantt-get-day-ratio down-end) 0)
                           (* -1.0 (- 1.0 (org-gantt-get-day-ratio down-end)))
                         0))
     (when (or (equal show-progress t)
               (and (equal show-progress 'if-clocksum)
                    clocksum))
       ;FIXME : use progress-prop here
       (concat
        ", progress="
        (if progress (number-to-string progress) "0")))
     "]"
     "{" (plist-get gi :name) "}"
     "{"
     (when up-start
       (format-time-string "%Y-%m-%d" up-start))
     "}"
     "{"
     (when down-end
       (format-time-string "%Y-%m-%d" down-end))
     "}"
     "\\\\%"
     (when start
       (format-time-string "%Y-%m-%d,%H:%M" start))
     " -- "
     (when effort
       (concat 
        (number-to-string (floor (time-to-number-of-days effort)))
        "d "
        (format-time-string "%H:%M" effort)))
     (when clocksum
       (concat 
        " -("
        (number-to-string (floor (time-to-number-of-days clocksum)))
        "d "
        (format-time-string "%H:%M" effort)
        ")- "))
     " -- "
     (when end
       (format-time-string "%Y-%m-%d,%H:%M" end))
     "\n"
     (when subelements
       (org-gantt-info-list-to-pgfgantt
	subelements
        show-progress
	(concat prefix "  ")
        (or ordered (plist-get gi :ordered)))))))

(defun org-gantt-info-list-to-pgfgantt (data &optional show-progress prefix ordered)
  "Gets a list of gantt-info-objects.
Returns a pgfgantt string representing that data."
  (concat
   (org-gantt-info-to-pgfgantt (car data) show-progress prefix ordered nil)
   (mapconcat (lambda (datum)
                (org-gantt-info-to-pgfgantt datum show-progress prefix ordered ordered))
              (cdr data)
              "")))

(defun org-gantt-days-to-vgrid-style (weekend workday weekend-style workday-style)
  (or
   (when weekend
     (concat "*" (number-to-string weekend) weekend-style))
   (when workday
     (concat "*" (number-to-string workday) workday-style))))

(defun org-gantt-get-vgrid-style (start-time weekend-style workday-style)
  "Computes a vgrid style from the start date, marking weekends."
  (let* ((dow (string-to-number (format-time-string "%w" start-time)))
         (weekend-start (and (or (= 0 dow) (> dow 4)) (% (- 8 dow) 7)))
         (work-start (and (> dow 0) (< dow 5) (- 5 dow)))
         (weekend-middle (and (not weekend-start) 3))
         (work-middle (and (not work-start) 4))
         (weekend-end (and weekend-start (< weekend-start 3) (- 3 weekend-start)))
         (work-end (and work-start (< work-start 4) (- 4 work-start))))
    (concat
     "{"
     (org-gantt-days-to-vgrid-style weekend-start work-start weekend-style workday-style)
     ","
     (org-gantt-days-to-vgrid-style weekend-middle work-middle weekend-style workday-style)
     (when (or weekend-end work-end) ",")
     (org-gantt-days-to-vgrid-style weekend-end work-end weekend-style workday-style)
     "}")))

(defun org-dblock-write:org-gantt-chart (params)
  "The function that is called for updating gantt chart code"
  (let (id idpos id-as-string view-file view-pos)
    (when (setq id (plist-get params :id))
      (setq id-as-string (cond ((numberp id) (number-to-string id))
			       ((symbolp id) (symbol-name id))
			       ((stringp id) id)
			       (t "")))
      (cond ((not id) nil)
	    ((eq id 'global) (setq view-pos (point-min)))
	    ((eq id 'local))
	    ((string-match "^file:\\(.*\\)" id-as-string)
	     (setq view-file (match-string 1 id-as-string)
		   view-pos 1)
	     (unless (file-exists-p view-file)
	       (error "No such file: \"%s\"" id-as-string)))))
    (with-current-buffer
        (if view-file
            (get-file-buffer view-file)
          (current-buffer))
      (org-clock-sum)
      (setq org-gantt-hours-per-day-gv (or (plist-get params :hours-per-day) org-gantt-default-hours-per-day))
      (let* ((titlecalendar (or (plist-get params :title-calendar) org-gantt-default-title-calendar))
             (start-date (plist-get params :start-date))
             (end-date (plist-get params :end-date))
             (start-date-list (and start-date (org-parse-time-string start-date)))
             (end-date-list (and end-date (org-parse-time-string end-date)))
             (start-date-time (and start-date-list (apply 'encode-time start-date-list)))
             (end-date-time (and end-date-list (apply 'encode-time end-date-list)))
             (additional-parameters (plist-get params :parameters))
             (weekend-style (or (plist-get params :weekend-style) org-gantt-default-weekend-style))
             (workday-style (or (plist-get params :workday-style) org-gantt-default-workday-style))
             (today-value (plist-get params :today))
             (show-progress (plist-get params :show-progress))
             (calc-progress (plist-get params :calc-progress))
             (id-subelements (plist-get params :use-id-subheadlines))
             (parsed-buffer (org-element-parse-buffer))
             (parsed-data
              (cond ((or (not id) (eq id 'global) view-file) parsed-buffer)
                    ((eq id 'local) (error "Local id handling not yet implemented"))
                    (t (org-element-map parsed-buffer 'headline
                         (lambda (element)
                           (if (equal (org-element-property :ID element) id) 
                               (if id-subelements
                                   (cdr element) 
                                 element)
                             nil))  nil t))))
             (org-gantt-info-list (org-gantt-crawl-headlines parsed-data))
             (org-gantt-check-info-list nil))
        (when (not parsed-data)
          (error "Could not find element with :ID: %s" id))
        (while (not (equal org-gantt-info-list org-gantt-check-info-list))
          (setq org-gantt-check-info-list (copy-tree org-gantt-info-list))
          (setq org-gantt-info-list
                (org-gantt-propagate-ds-up
                 (org-gantt-propagate-order-timestamps
                  (org-gantt-calculate-ds-from-effort org-gantt-info-list)))))
;        (message "%s" org-gantt-info-list)
        (setq org-gantt-info-list (org-gantt-propagate-summation-up 
                                   org-gantt-info-list
                                   org-gantt-effort-prop
                                   #'org-gantt-get-subheadline-effort-sum))
        (setq org-gantt-info-list (org-gantt-compute-progress org-gantt-info-list))
        (message "%s" (pp org-gantt-info-list))
        (setq org-gantt-info-list (org-gantt-propagate-summation-up
                                   org-gantt-info-list
                                   org-gantt-progress-prop
                                   (lambda (hl) (org-gantt-get-subheadline-progress-summation hl calc-progress t)) 
                                   t))
        ;;FIXME: use org-gantt-info-list instead of parsed data.
        (setq start-date-time 
              (or start-date-time 
                  (org-gantt-get-extreme-date parsed-data #'org-gantt-get-start-time #'org-gantt-time-less-p)))
        (setq end-date-time 
              (or end-date-time
                  (org-gantt-get-extreme-date parsed-data #'org-gantt-get-end-time
                                              (lambda (t1 t2) (not (org-gantt-time-less-p t1 t2))))))
        (insert
         (concat
          "\\begin{ganttchart}[time slot format=isodate, "
          "vgrid="
          (org-gantt-get-vgrid-style start-date-time weekend-style workday-style)
          (when today-value
            (concat
             ", today="
             (format-time-string
              "%Y-%m-%d"
              (if (equal t today-value)
                  (current-time)
                (org-gantt-timestamp-to-time (org-parse-time-string today-value))))))
          (when additional-parameters
            (concat ", " additional-parameters))
          "]{"
          (format-time-string "%Y-%m-%d" start-date-time)
          "}{"
          (format-time-string "%Y-%m-%d" end-date-time)
          "}\n"
          "\\gantttitlecalendar{"
          (if titlecalendar
              titlecalendar
            "year, month=name, day")
          "}\\\\\n"
          (org-gantt-info-list-to-pgfgantt org-gantt-info-list show-progress)
          "\\end{ganttchart}"))))))

(provide 'org-gantt)

;;; org-gantt-experiment.el ends here
