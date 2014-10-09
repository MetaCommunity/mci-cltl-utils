
(in-package #:utils)

(deftype file-name ()
  `(or string pathname))

(deftype file-designator ()
  `(or string pathname file-stream))

(deftype class-designator ()
  '(or symbol class))

(defun compute-class (ident &optional (errorp t)
                              environment)
  (declare (type class-designator ident)
           (values (or class null)))
  (etypecase ident
    (symbol (find-class ident errorp environment))
    (class ident)))

;; trivial tests for COMPUTE-CLASS
;; (compute-class 'string)
;; (compute-class (find-class 'ratio))
;; (compute-class (class-of (1+ most-positive-fixnum)))
