;;;; package.lisp

(defpackage #:cl-org-utils
  (:use #:cl)
  (:import-from #:net.telent.date #:parse-time)
  (:import-from #:cl-ppcre #:scan)
  (:export #:org-date #:encode-org-time #:decode-org-time
           #:org-date-p
           #:process-row #:output-table #:input-table)
  (:documentation "Collection of utils to work with CL from ORG-MODE files"))

