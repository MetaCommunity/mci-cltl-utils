;; common-misc.lisp - lisp compiler integration (misc)
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/common)


;;
;; condition types originally defined in common-opt.lisp
;;

(define-condition compile-condition ()
  ())


(define-condition compile-warning  (warning compile-condition)
  ())


(define-condition compile-error (error compile-condition)
  ;; NB: NAME COLLISION w/ ASDF/BACKWARD-INTERFACE:COMPILE-ERROR via :ASDF
  ())

;;
;; condition types originally defined in common-misc.lisp (this file)
;;

(define-condition function-compile-condition (compile-condition)
  ((function-name
    :initarg :function-name
    :initform nil
    :reader fun-compile-condition-function-name)
   (lambda-form
    :initarg :lambda-form
    :initform nil
    :reader fun-compile-condition-lambda-form))
  (:report
   ;; NB: Quirky, counterintiutive semantics of using a :REPORT SYMBOL
   ;; expression in DEFINE-CONDITION [CLtL2]
   ;;
   ;; TD: DEFMACRO DEFINE-CONDITION* such that will provide a default
   ;; :REPORT function as below, when none is specified
   (lambda (c s)
     (format-condition c s))))

(defmethod format-condition ((c function-compile-condition)
                             (s stream))
  (format (the stream s)
          ;; FIXME spurious "for" when both nil
          "Unknown ~S for~@[ ~S~]~@[ form: ~S~]"
          (class-name (class-of c))
	  (fun-compile-condition-function-name c)
	  (fun-compile-condition-lambda-form c)))


;; ----

(define-condition warnings-during-function-compile
    (compile-warning function-compile-condition)
  ())

(defmethod format-condition ((c warnings-during-function-compile)
                             (s stream))
  (format (the stream s)
          ;; FIXME spurious "of" when both nil
          "Warnings during compilation of~@[ ~S~]~@[ form: ~S~]"
	  (fun-compile-condition-function-name c)
	  (fun-compile-condition-lambda-form c)))

#+NIL
(eval-when ()
  ;; initial debug, cf DEFINE-CONDITION :REPORT SYMBOL QUIRKS

  (let ((*print-escape* t))
    (warn 'warnings-during-function-compile
        :function-name 'nope
        :lambda-form "N/A"))

  (let ((*print-escape* nil))
    (warn 'warnings-during-function-compile
          :function-name 'nope
          :lambda-form "N/A"))
  )


;; ----

(define-condition errors-during-function-compile
    (compile-error function-compile-condition)
  ())


(defmethod format-condition ((c errors-during-function-compile)
                             (s stream))
  (format (the stream s)
          ;; FIXME spurious "of" when both nil
          "Errors during compilation of ~@[~S~]~@[form: ~S~]"
	  (fun-compile-condition-function-name c)
	  (fun-compile-condition-lambda-form c)))


;; Compilation Wrapper Forms


;; NOTE: Define WITH-CONDITION-CACHING (??)
;;
;; NB: Without binding a condition handler for WARN and/or ERROR, it may
;; not otherwise be possible - portably - to capture the original
;; condition objects for representation to the user - or in the system
;; log - when there are warnings or errors during compilation of a
;; function, as via COMPILE* or COMPILE**
;;
;; Each of WARNINGS-DURING-FUNCTION-COMPILE and ERRORS-DURING-FUNCTION-COMPILE
;; may be used within an environment such that would serve to establish
;; a lexical binding -- via a condition handler and lexically scoped
;; variable for each of error and  warning conditions -- subsequently
;; presenting that value via the corresponding condition :REPORT lambda.
;;
;; Such an environment may be used to wrap the internal COMPILE and
;; subsequent condtional handlers in each of COMPILE* and COMPILE**
;; below.
;;
;; Note that the specific condition of ERROR during COMPILE may require
;; some further handling, other than simply caching the value into a
;; lexically scoped buffer. If an ERROR may indicate a situation such
;; that should prevent further compilation of a form, that condition
;; should be not only cached but furthermore handled in a way as to
;; allow the user to prevent further compilation (use CERROR)
;;
;; Note that this -- insofar as being applied with COMPILE* and
;; COMPILE** forms -- this, in itself, would not serve to address
;; warnings and errors during COMPILE-FILE. In the interest of
;; interactive debugging, it may be useful to integrate the resulting
;; WITH-CONDITION-CACHING behaviors with procedures used by any single
;; IDE system.


;; --

(defmacro compile** (form &optional optimization)
  ;; Just-in-time compilation for anonymous lambda forms -
  ;; defined such as to be evaluated within the calling lexical
  ;; environment.
  (with-symbols (%form fn warnedp failurep)
    `(with-optimization (,@optimization)
       (let ((,%form ,form))
         (multiple-value-bind  (,fn ,warnedp ,failurep)
             (compile nil ,%form)
           (when ,warnedp
             (warn 'warnings-during-function-compile
                   :function-name nil
                   :lambda-form ,%form))
           (cond
             (,failurep
              (cerror "Ignore Errors"
                      "Errors when compiling ~S" ,%form
                      (make-condition 'errors-during-function-compile
                                      :function-name nil
                                      :lambda-form  (quote ,%form))))
              (values ,fn))
             (t (values ,fn)))))))

;; Instance Failure Tests

#+nil
(eval-when ()
  (let* ((form '(lambda () (foo bar)))
         (fn (compile** form (debug 3)))
         (bar (quote bar)))

    (warn "...")

    (labels ((foo (arg) (list 'foo!? arg)))
      ;; (funcall fn)
      ;; ^ fails - Hypothesis: Original FN compilation environment does
      ;;   not include a definition of #'FOO. Any subsequent definition
      ;;   of #'FOO may not affect the compiled definition. (??)
      (let ((bar 'bar))
        (funcall (compile** (copy-list form)))
        ;; ^ fails, but ideally should not. #'FOO and BAR are both bound
        ;; in this environment.
        ;;
        ;; TBD: Why the fail [SBCL]
        )))
)

;; (compile** '(lambda () (cl:print)) (debug 3))

;; (compile** '(lambda () (+ 1 a)))




(defun* compile* (name &optional defn)
  ;; FIXME: Align onto the error/waning handling semantics of COMPILE**
  ;; see opt-utils.lisp
  "Evalute COMPILE on NAME and DEFN.

If COMPILE indicates errors during compilation, an error of type
`ERRORS-DURING-COMPILE' is produced. It is assumed that this will always
result in a non-local exit of control flow.

If COMPILE indicate warnings during compilation, a warning of type
`WARNINGS-DURING-COMPILE' is produced. If the warning is not dispatched
to a non-local exit of control flow, then the compiled function is
returned.

In both instances, the resulting condition object will contain
information about -- in a respective regard -- the compiler errors or
compiler warnings produced.

In other instances, the compiled function is returned."
  (declare (type function-designator name)
	   (values function &optional))
  (multiple-value-bind (fn warnings-p failure-p)
      (compile name defn)
      (cond
	(failure-p
	 (error 'errors-during-function-compile
		:function-name name
		:lambda-form defn))
	(warnings-p
	 ;; Note that this may effectively produce a full WARNING out of
         ;; a STYLE-WARNING. See also: HANDLER-CASE, HANDLER-BIND
	 (warn 'warnings-during-function-compile
	       :function-name name
	       :lambda-form defn)
	 (values fn))
	(t (values fn)))))



;; (compile* nil '(lambda () "FOO"))
;;;
;; (compile* nil '(lambda () unbound-foo))

