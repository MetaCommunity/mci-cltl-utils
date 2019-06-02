;; misc-utils.lisp - assorted utility forms
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

;;; % COMPILE*

;; FIXME This overlaps w/ some feature sets in ltp-common
;; cf. ./common-opt.lisp

(define-condition compilation-condition ()
  ((function-name
    :initarg :function-name
    :reader compilation-condition-function-name)
   (lambda-form
    :initarg :lambda-form
    :reader compilation-condition-lambda-form)))

(define-condition warnings-during-compile (warning compilation-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Compiler warned during compilation~@[ of ~S~]~@[ form: ~S~]"
	     (compilation-condition-function-name c)
	     (compilation-condition-lambda-form c)))))

(define-condition errors-during-compile (error compilation-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Errors during compilation~@[ of ~S~]~@[ form: ~S~]"
	     (compilation-condition-function-name c)
	     (compilation-condition-lambda-form c)))))



(defun* compile* (name &optional defn)
  ;; FIXME: Align onto the error/waning handling semantics of COMPILE**
  ;; see opt-utils.lisp
  "Evalute COMPILE on NAME and DEFN.

If COMPILE indicates errors during compilation, an error of type
`ERRORS-DURING-COMPILE' is signaled.

If COMPILE indicate warnings during compilation, a warning of type
`WARNINGS-DURING-COMPILE` is signaled, and the compiled function is
returned.

In other instances, the compiled function is returned."
  (declare (type function-designator name)
	   (values function &optional))
  (multiple-value-bind (fn warnings-p failure-p)
      (compile name defn)
      (cond
	(failure-p
	 (error 'errors-during-compile
		:function-name name
		:lambda-form defn))
	(warnings-p
	 ;; Note that this effectively makes a full WARNING out of a
	 ;; STYLE-WARNING, with anything short of a full HANDLER-CASE
	 ;; or such
	 (warn 'warnings-during-compile
	       :function-name name
	       :lambda-form defn)
	 (values fn))
	(t (values fn)))))


;; (compile* nil '(lambda () "FOO"))
;; (compile* nil '(lambda () unbound-foo))


(deftype string-designator ()
  '(or symbol string))

(deftype package-designator ()
  ;; cf. DEFPACKAGE [CLtL2]
  '(or string-designator package))

(defun* package-exports-symbols (pkg)
  (declare (type package-designator pkg)
           (values list &optional))
  (let ((buffer
         (make-array 8 :fill-pointer 0)))
    (declare (type (array t (*)) buffer))
    (do-external-symbols (s pkg  (coerce buffer 'list))
      (vector-push-extend s buffer))))

;; Test form (ad hoc):
;; (package-exports-symbols '#:c2mop)
