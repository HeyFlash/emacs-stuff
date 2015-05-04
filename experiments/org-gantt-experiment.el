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

(defun org-gantt-has-effort (aplist)
  "returns non-nil if an alist or plist contains effort"
(or (plist-get aplist ':effort)
    (assoc "effort" aplist)))

(defun org-gantt-has-startdate (aplist)
  "returns non-nil if an alist or plist contains a startdate"
(or (plist-get aplist ':scheduled)
    (assoc "SCHEDULED" aplist)))

(defun org-gantt-has-enddate (aplist)
  "returns non-nil if an alist or plist contains an enddate"
  (or (plist-get aplist ':deadline)
      (assoc "DEADLINE" aplist)))

(defun org-gantt-identity (x) x)

(defun org-gantt-get-planning-timestamp (element timestamp-type)
  "Get the timestamp from the planning element that belongs to a first-order headline
of the given element. timestamp-type is either ':scheduled or ':deadline"
(org-element-map element '(planning headline) 
  (lambda (subelement) (org-element-property timestamp-type subelement))
  nil t 'headline))

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
             (org-gantt-tp-compare ts1 ts2 ':year-start #'<))


           ;; (org-gantt-tp< ts1 ts2 ':year-start)
           ;; (org-gantt-tp< ts1 ts2 ':month-start)
           ;; (org-gantt-tp< ts1 ts2 ':day-start)
           ;; (org-gantt-tp< ts1 ts2 ':hour-start)
           ;; (org-gantt-tp< ts1 ts2 ':minute-start)
           )))

(defun org-gantt-timestamp-larger (ts1 ts2)
  "Returns true iff not timestamp ts1 is before ts2"
  (not (org-gantt-timestamp-smaller ts1 ts2)))

(defun org-gantt-subheadline-extreme (element comparator time-getter)
  "Returns the smallest/largest timestamp of the subheadlines 
of element according to comparator.
time-getter is the recursive function that needs to be called if 
the subheadlines have no timestamp."
  (and
   element
   (let ((subheadlines (org-gantt-get-subheadlines element)))
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
 (org-gantt-get-planning-timestamp element ':scheduled)
 (org-gantt-subheadline-extreme
  (cdr element)
  #'org-gantt-timestamp-smaller
  #'org-gantt-get-start-time)))

(defun org-gantt-get-end-time (element)
""
(or
 (org-gantt-get-planning-timestamp element ':deadline)
 (org-gantt-subheadline-extreme
  (cdr element)
  (lambda (ts1 ts2)
    (not (org-gantt-timestamp-smaller ts1 ts2)))
  #'org-gantt-get-end-time)))

(defun org-gantt-create-gantt-info (element)
  "Creates a gantt-info for element.
A gantt-info is a plist containing :name :start :end"
  (list ':name (org-element-property ':raw-value element)
	':ordered (org-element-property ':ordered element)
        ':start (org-gantt-get-start-time element)
        ':end (org-gantt-get-end-time element)
        ':subelements (org-gantt-crawl-headlines (cdr element) nil nil nil)))

;; (defun org-gantt-subpropagate-start-date (element start-date)
;;   "Propagates end date from one subelement to start date of next subelement,
;; if element is ordered."
;;   (let ((subelements (plist-get element :subelements))
;; 	(ordered (plist-get element :ordered)))
;;     (if ordered
;; 	(dolist (se subelements)
;; 	  (let ((end (plist-get se :end)))
;; 	    (if end
;; 		))))))

(defun org-gantt-crawl-headlines (data parent-start parent-end sorted)
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
     (number-to-string (plist-get ts ':day-start))
     )))

(defun org-gantt-info-to-pgfgantt (gi &optional prefix)
  "Creates a pgfgantt string from gantt-info gi"
  (let ((subelements (plist-get gi ':subelements)))
    (concat
     prefix
     (if subelements
	 "\\ganttgroup"
       "\\ganttbar")
     "{" (plist-get gi ':name) "}"
     "{"
     (when (plist-get gi ':start)
       (org-gantt-timestamp-to-string (plist-get gi ':start)))
     "}"
     "{"
     (when (plist-get gi ':end)
       (org-gantt-timestamp-to-string (plist-get gi ':end)))
     "}"
     "\\\\\n"
     (when subelements
       (org-gantt-info-list-to-pgfgantt
	subelements
	(concat prefix "  "))))))

(defun org-gantt-get-extreme-date (data time-getter timestamp-comparer)
  "Get the first or last date (depending on timestamp-comparer) 
of all the timestamps in data"
  (let ((timestamps
         (org-element-map data 'headline 
           (lambda (headline) 
             (funcall time-getter headline)))))
    (car (sort timestamps timestamp-comparer))))

(defun org-gantt-info-list-to-pgfgantt (data &optional prefix)
  "Gets a list of gantt-info-objects.
Returns a pgfgantt string representing that data."
  (mapconcat (lambda (datum) (org-gantt-info-to-pgfgantt datum prefix)) data ""))


(defun org-dblock-write:pgf-gantt-chart (params)
  "The function that is called for updating gantt chart code"
  (let* ((titlecalendar (plist-get params :titlecalendar))
         (start-date (plist-get params :startdate))
         (end-date (plist-get params :enddate))
         (additional-parameters (plist-get params :parameters))
         (parsed-data (org-element-parse-buffer))
         (org-gantt-info-list (org-gantt-crawl-headlines parsed-data nil nil nil)))
    (insert
     (concat
      "\\begin{ganttchart}[time slot format=isodate"
      (when additional-parameters 
        (concat ", " additional-parameters))
      "]{" 
      (or start-date
          (org-gantt-timestamp-to-string 
           (org-gantt-get-extreme-date parsed-data #'org-gantt-get-start-time #'org-gantt-timestamp-smaller)))
      "}{"
      (or end-date
          (org-gantt-timestamp-to-string
           (org-gantt-get-extreme-date parsed-data #'org-gantt-get-end-time #'org-gantt-timestamp-larger))) 
      "}\n"
      "\\gantttitlecalendar{"
      (if titlecalendar
          titlecalendar
        "year, month=name, day")
    " }\\\\\n"
    (org-gantt-info-list-to-pgfgantt org-gantt-info-list)
    "\\end{ganttchart}"))))
  
