;; misc-utils.lisp - assorted utility forms

(in-package #:utils)

(defun print-hash-table (table &optional 
                                 (stream *standard-output*)
                         &key (format-control 
                               (load-time-value 
                                (compile nil (formatter "~S : ~S")))))
  (declare (type hash-table table)
           (type stream-designator stream))
  (maphash (lambda (k v)
             (format stream format-control k v))
           table))

