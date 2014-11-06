;; stream-utils.lisp - stream utilities

(in-package #:utils)

(deftype stream-designator ()
  '(or (and symbol (member nil t)) stream 
    (and string (satisfies array-has-fill-pointer-p))))


#+NIL (typep (make-array 20 :fill-pointer 0 :element-type 'base-char)
	     'stream-designator)
;; => T


(deftype format-control ()
  '(or string function))


(defun compute-output-stream (s)
  "Given a stream designator, S, return:

* S if S is a STREAM
* *STANDARD-OUTPUT* if S is NULL
* *TERMINAL-IO* if S is EQ to T

See also: `compute-input-stream'"
  (declare (type stream-designator s)
           (values stream))
  (etypecase s
    (stream (values s))
    ((nil) (values *standard-output*))
    ((t) (values *terminal-io*))))
  

(defun compute-input-stream (s)
  "Given a stream designator, S, return:

* S if S is a STREAM
* *STANDARD-INPUT* if S is NULL
* *TERMINAL-IO* if S is EQ to T

See also: `compute-output-stream'"
  (declare (type stream-designator s)
           (values stream))
  (etypecase s
    (stream (values s))
    ((nil) (values *standard-input*))
    ((t) (values *terminal-io*))))

