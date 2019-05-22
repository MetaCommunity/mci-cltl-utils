;; stream-utils.lisp - stream utilities
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/common)

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
    (null (values *standard-output*))
    ((eql t) (values *terminal-io*))))


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
    (null (values *standard-input*))
    ((eql t) (values *terminal-io*))))

