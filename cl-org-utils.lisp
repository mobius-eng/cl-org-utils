(in-package #:cl-org-utils)

;; * CL-ORG-UTILS
;; ** Parse ORG time stamps
(defvar *org-date-regular-expression*
  "^<([0-9]{4})-([0-9]{2})-([0-9]{2})[\\ ]*[a-zA-Z]*[\\ ]*([0-9]{2}:[0-9]{2})?-?([0-9]{2}:[0-9]{2})?>$"
  "String representing the regular exression for ORG date format, e.g.
  <2016-02-15 Mon 13:00-17:00> ")

(defun get-date (org-time-string begins ends)
  "Extracts the date out of parsed ORG time string"
  (let ((year (parse-integer (subseq org-time-string (aref begins 0) (aref ends 0))))
        (month (parse-integer (subseq org-time-string (aref begins 1) (aref ends 1))))
        (day (parse-integer (subseq org-time-string (aref begins 2) (aref ends 2)))))
    (list year month day)))


(defun get-time (org-time-string begins ends)
  "Extracts time from parsed ORG time string"
  (let ((hour (parse-integer (subseq org-time-string
                                     (aref begins 3)
                                     (+ (aref begins 3) 2))))
        (minute (parse-integer (subseq org-time-string
                                       (+ (aref begins 3) 3)
                                       (aref ends 3)))))
    (list hour minute)))

(defun encode-org-time (org-time-string)
  "Encodes ORG time string as a universal time value"
  (multiple-value-bind (beg end begins ends)
      (scan *org-date-regular-expression* org-time-string)
    (declare (ignore end))
    (when beg
      (destructuring-bind (year month day) (get-date org-time-string begins ends)
        (if (aref begins 3)
            (destructuring-bind (hour minute) (get-time org-time-string begins ends)
              (apply #'encode-universal-time (list 0 minute hour day month year)))
            (apply #'encode-universal-time (list 0 0 0 day month year)))))))

(defvar *day-week*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun decode-org-time (time &optional (out *standard-output*))
  "Decode universal time to ORG time string"
  (declare (special *day-week*))
  (multiple-value-bind (second minute hour day month year week-day)
      (decode-universal-time time)
    (declare (ignore second))
    (if (and (zerop minute) (zerop hour))
        (format out "<~4,'0D-~2,'0D-~2,'0D ~A>"
                year month day (svref *day-week* week-day))
        (format out "<~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D>"
                year month day (svref *day-week* week-day) hour minute))))

(defclass org-date ()
  ((org-date-internal :initarg :internal :reader org-date-internal))
  (:documentation "CL representation of ORG-MODE date (time stamp)"))

(defun org-date (org-time-string)
  "Construct ORG-DATE from ORG-MODE time samp string"
  (make-instance 'org-date :internal (encode-org-time org-time-string)))

(defmethod print-object ((obj org-date) out)
  (decode-org-time (org-date-internal obj) out))

(defun org-date-p (string)
  "Tests if a given string is an apropriate ORG-MODE time stamp string"
  (encode-org-time string))

;; ** ORG tables from Common Lisp
;; By default, CL does not produce nicely formatted tables
;; to be used in ORG-MODE. One of the problems: symbols are
;; printed in UPPERCASE, thus indicator =hline= becomes
;; =HLINE= and does not make a horisontal line
;; Solution: make tables as outputs rather than values

(defgeneric pre-process (item))

(defmethod pre-process ((item t)) item)

(defmethod pre-process ((item double-float))
  (coerce item 'single-float))

(defun process-row (row)
  "Convert each item in the row to aesthetic string"
  (cond ((consp row)
         (mapcar (lambda (item)
                   (with-output-to-string (out)
                     (princ (pre-process item) out)))
                 row))
        ((symbolp row)
         (string-downcase (with-output-to-string (out)
                            (princ row out))))
        (t (with-output-to-string (out)
             (princ (pre-process row) out)))))

(defun output-table (table &optional (hline t) (out *standard-output*))
  "Produces table output. If HLINE is T, put horisontal line between the
   first row (column titles) and the rest.
   The table may contain other 'hline' inside"
  (let ((string-table (mapcar #'process-row
                              (if hline
                                  (list* (first table) 'hline (rest table))
                                  table))))
    (princ "(" out)
    (dolist (row string-table)
      (if (consp row)
          (prin1 row out)
          (princ row))
      (princ " " out))
    (princ ")" out)))

(defun input-table (org-table)
  "Inputs ORG-MODE table, converting time stamps to ORG-DATE objects"
  (mapcar (lambda (row)
            (mapcar (lambda (item)
                      (cond ((numberp item) item)
                            ((and (stringp item) (org-date-p item))
                             (org-date item))
                            (t item)))
                    row))
          (remove-if-not #'consp org-table)))
