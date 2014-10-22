;; stream-utils.lisp - stream utilities

(in-package #:utils)

(deftype stream-designator ()
  '(or (and symbol (member nil t)) stream 
    (and string (satisfies array-has-fill-pointer-p))))


;; (typep (make-array 20 :fill-pointer 0 :element-type 'base-char) 'stream-designator)
;; => T


(deftype format-control ()
  '(or string function))
