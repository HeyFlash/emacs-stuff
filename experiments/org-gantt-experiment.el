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

(defun org-gantt-get-start-time (element)
""
(or
 (org-gantt-get-planning-timestamp element ':scheduled)
 ;; here take min of org-gantt-get-start-time of all subheadlines.
 ))

(defun org-gantt-get-end-time (element)
""
(or
 (org-gantt-get-planning-timestamp element ':deadline)
 ;;here take max of gantt-get-end-time of all subheadlines.
 ))

(defun org-gantt-create-gantt-info (element)
  "Creates a gantt-info for element.
A gantt-info is a plist containing :name :start :end"
  (list ':name (org-element-property ':raw-value element)
        ':start (org-gantt-get-start-time element)
        ':end (org-gantt-get-end-time element)
        ':subelements (org-gantt-crawl-headlines (cdr element) nil nil nil)))

(defun org-gantt-crawl-headlines (data parent-start parent-end sorted)
""
(org-element-map data 'headline #'org-gantt-create-gantt-info nil nil 'headline))
