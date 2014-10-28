;; print-utils.lisp - utilities for object printing

(in-package #:utils)

(defgeneric (setf object-print-name) (new-value object))
(defgeneric (setf object-print-label) (new-value object))

(defgeneric object-print-name (object)
  ;; FIXME: #I18N

  ;; FIXME: Generic function FTYPE declarations
  ;; => ftype (function (t) simple-string)

  (:documentation
   "Return a unique, human-readable name for OBJECT

See also: `object-print-label'")
  ;; FIXME: This updates some qualities of the dobelle-app source tree

  (:method (object)
    (prin1-to-string object))
  (:method ((object string))
    (values object))
  (:method ((object package))
    (package-name object))
  (:method ((object symbol))
    (multiple-value-bind (status pkg)
	(symbol-status object)
      (let ((keyword-p
	     (and pkg (eq pkg (find-package '#:keyword)))))
	(with-output-to-string (s)
	  (cond
	    (keyword-p
	     (write-char #\: s))
	    (pkg (princ (object-print-name pkg) s))
	    (t (write-char #\# s)
	       (write-char #\: s)))
	  (case status
	    (:external
	     (unless keyword-p
	       (write-char #\: s)))
	    ((:internal :inherited)
	     (write-char #\: s)
	     (write-char #\: s)))
	  (write-string (symbol-name object) s)))))
  (:method ((object class))
    (object-print-name (class-name object)))
  )

;; (object-print-name (find-package '#:utils))
;; => "INFO.METACOMMUNITY.CLTL.UTILS"
;; (object-print-name '#:foo)
;; => "#:FOO"
;; (object-print-name 'print)
;; => "COMMON-LISP:PRINT"
;; (object-print-name ':test)
;; = ":TEST"

(defgeneric object-print-label (object)
  ;; FIXME: This updates some qualities of the dobelle-app source tree

  ;; FIXME: Generic function FTYPE declarations
  ;; => ftype (function (t) simple-string)
  (:documentation 
   "Return a succinct, unique, human-readable label for OBJECT

See also: `object-print-name'")
  (:method (object)
    (princ-to-string object))
  (:method ((object string))
    (values object))
  (:method ((object package))
    (let ((name (package-name object))
	  (nicknames (package-nicknames object)))
      (car (sort (cons name (copy-list nicknames))
		 #'(lambda (a b)
		     (declare (type simple-string a b))
		     (< (length a) (length b)))))))
  (:method ((object symbol))
    (multiple-value-bind (status pkg)
	(symbol-status object)
      (let ((keyword-p
	     (and pkg (eq pkg (find-package '#:keyword)))))
	(with-output-to-string (s)
	  (cond
	    (keyword-p
	     (write-char #\: s))
	    (pkg (princ (object-print-label pkg) s))
	    (t (write-char #\# s)
	       (write-char #\: s)))
	  (case status
	    (:external
	     (unless keyword-p
	       (write-char #\: s)))
	    ((:internal :inherited)
	     (write-char #\: s)
	     (write-char #\: s)))
	  (write-string (symbol-name object) s)))))
  (:method ((object class))
    (object-print-label (class-name object)))
  )

;; (object-print-label (find-package '#:utils))
;; => "UTILS"
;; (object-print-label '#:foo)
;; => "#:FOO"
;; (object-print-label 'print)
;; => "CL:PRINT"
;; (object-print-label ':test)
;; => ":TEST"
;; (object-print-label (find-class 'stream))
;; => "CL:STREAM"

(defclass pretty-printable-object ()
  ;; The 'label' and 'name' arguments might seem redundant.
  ;; This class was defined, originally, as a mixin for
  ;; application in the igneous-math system
  ((print-label
    :initarg :print-label
    :type simple-string
    :accessor object-print-label)
   (print-name 
    :initarg :print-name
    :type simple-string
    :accessor object-print-name))
   (:documentation 
    "Mixin class for OBJECT-PRINT-LABEL, OBJECT-PRINT-NAME functions"))


(defgeneric format-label (stream arg colon-p at-p)
  ;;; FIXME: Handle pprint-dipstach, etc
  (:method (stream arg colon-p at-p)
    (declare (ignore colon-p))
    (cond
      (at-p 
       (princ (object-print-name arg) stream))
      (t
       (princ (object-print-label arg) stream)))))


(defun print-hash-table (table 
                         &key
			   (stream *standard-output*)
			   (format-control 
			    (load-time-value 
			     (compile nil 
				      (formatter "~%~/utils:format-label/ : ~/utils:format-label/")))))
  (declare (type hash-table table)
           (type stream-designator stream))
  (maphash (lambda (k v)
             (format stream format-control k v))
           table))

;; (with-output-to-string (*standard-output*) (print-hash-table asdf::*defined-systems*))
