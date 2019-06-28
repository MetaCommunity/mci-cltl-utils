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


(declaim (ftype (function ((or symbol cons))
                          (values boolean (or symbol cons) (or symbol cons)
                                  &optional))
                featurep))

(defun featurep (expr)
  ;;
  ;; NB: This function does not provide support for *FEATURES* elements
  ;; interned in any CL:PACKAGE not equivalent to either *PACKAGE* or
  ;; the :KEYWORD package
  ;;
  ;; --
  ;;
  ;; first return value:
  ;;  a boolean indicating whether the EXPR is interpreted as
  ;;  representing a true feature expression
  ;;
  ;; second return value:
  ;;  an inner-most element within EXPR that did not evaluate as a true
  ;;  feature expresion
  ;;
  ;; third return value
  ;;  an inner-most element within EXPR that evaluated as a true feature
  ;;  expression
  ;;
  ;;
  ;; NB: Return values of this function should not be destructively
  ;;     modified, as they may represent constant data.
  ;;
  ;; --
  ;;
  ;; The second and third return values are provided in an interest of
  ;; systems debugging, for nested feature expressions
  ;;
  (let (%mangled-name%)
    (declare (type symbol %mangled-name%))
    (labels ((mangle-name-kwd (name)
               (setq %mangled-name%
                     (intern (symbol-name name)
                             (load-time-value (find-package :keyword)
                                              t))))
             (feature-declared-p (name)
               ;; FIXME: Implementation of a name mangling function
               ;; providing behaviors similar to CLtL2 reader macros #\#\+
               ;; and #\#\-
               ;;  - would have to be implemented onto I/O streams
               ;;  - may serve to support *FEATURES* expressions not
               ;;    interned in the #:KEYWORD package
               ;;  - is not provided here
               (cond
                 ((find name *features* :test #'eq)
                  ;; NB: The first check for any symbol expr is
                  ;; performed, in this function, using that symbol's
                  ;; initial SYMBOL-PACKAGE
                  (values t nil name))
                 ((find (mangle-name-kwd name) *features* :test #'eq)
                  ;; Subsequently, check for a symbol with equivalent
                  ;; SYMBOL-NAME, in the :KEYWORD package.
                  ;;
                  ;; This labels function returns the keyword symbol, if found.
                  ;;
                  ;; NB: That symbol might be - in effect - lost for
                  ;; some purposes of top-level return value capture, in
                  ;; the methodology of return-value capture for feature
                  ;; expressions, as developed internally in this function.
                  ;; Regardless, it should be visible to some (TRACE FEATUREP)
                  ;;
                  (values t nil %mangled-name%))
                 (t (values nil name nil))))
             (and-declared-p (exprs)
               (dolist (expr (cdr exprs) (values t nil exprs))
                 (multiple-value-bind (decl-p fail pass)
                     (featurep expr)
                   (declare (ignore pass))
                   (unless decl-p
                     (return (values nil fail nil))))))
             (or-declared-p (exprs)
               (dolist (expr (cdr exprs) (values nil exprs nil))
                 (multiple-value-bind (decl-p fail pass)
                     (featurep expr)
                   (declare (ignore fail))
                   (when decl-p
                     (return (values t nil pass)))))))
      (etypecase expr
        (symbol (feature-declared-p expr))
        (cons
         (let ((frst (car expr)))
           (ecase frst
             (and (and-declared-p expr))
             (or (or-declared-p expr))
             (not (multiple-value-bind (decl-p fail pass)
                      (feature-declared-p (cadr expr))
                    (declare (ignore fail pass))
                    (cond
                      (decl-p (values nil expr nil))
                      (t (values t nil expr))))))))))))

;; FIXME: Note the topic, Global Locking During *FEATURES* Eval,
;; in project doc/markdown/ src. FEAUTREP should not be assumed
;; to be safe for application during concurrent evaluation of arbitrary
;; source forms such that may modify the value of *FEATURES*


;; (featurep 'sbcl)
;; =[SBCL]=> T, NIL, :SBCL

;; NB - by side effect
;; (featurep '#:sbcl)
;; =[SBCL]=> T, NIL, :SBCL

;; (featurep '(not sb-thread))

;; (featurep '(and sbcl sb-thread))

;; (featurep '(and openmcl cmucl))

;; (featurep '(and :sbcl :sb-thread))

;; (featurep '(and :sbcl (not :sb-thread)))

;; (featurep '(and :ansi-cl (or :unsupp-1 :unsupp-2)))

;; (featurep '(and :ansi-cl (and (not :unix) :unsupp-2)))

;; (featurep '(and :ansi-cl (or (not :unix) :unsupp-2)))

;; (featurep '(and :ansi-cl (or :unsupp-2 :unix)))

;; (featurep '(and :ansi-cl (and :unsupp-2 :unix)))

;; --

(defmacro mk-lf (form)
  "Evaluate FORM within a constantp LOAD-TIME-VALUE expression"
  `(load-time-value ,form t))

;; ----------
;;
;; NB: Prototype forms, defined below, may be subject to deletion in
;; subsequent revions of this system
;;
;; ----------

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
  ;; FIXME: Portable "In what section" declaration
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

;; NB: Some general considerations being approached, in whatsover, with
;; the following ad hoc source forms:
;;
;; - Policy - namely, optimizatoin policy - during runtime COMPILE
;; - Call to COMPILE specifically for anonymous LAMBDA forms
;; - User-visible presentation for messages produced during runtime
;;   COMPILE (warning, or error)
;;
;; The last of those concerns has not been very thoroughly approached,
;; below. Rather than establishing any handler-bind for ERROR and/or
;; WARNING, the forms simply rely on the return-value from COMPILE
;;
;; Not addressed, below:
;; - Selection of an appropriate, non-null lexical environment for the
;;   compilation - such that may require implementation-specific
;;   support, in any application.
;;
;; NB: These forms may be of some utility for applications producing
;; anonymous LAMBDA forms at runtime, namely as for any subsequent
;; FUNCALL. e.g
;;
;; - Extensions onto MOP, such as with regards to class initialization
;;   with slot definitions in MOP, insofar as concerning slot definition
;;   initfunction forms
;;
;; - Other application-specific initfunction forms - e.g for
;;   initialization of default parameter values, for some function-like
;;   procedural calls
;;
;; - Arbitrary callback procedures, as operating onto implementation-
;;   specific support for external call stacks, such as vis a vis
;;   the ALIEN system in CMUCL, SB-ALIEN in SBCL, and any analogous
;;   external object systems interface in other Common Lisp
;;   implementations.
;;
;; Ed. NB: Albeit, these forms may appear to be defined as to avoid
;; some matters of fucntionality, in lieu of producing a generally
;; portable interface for application programs.


;; TBD: Consider definiing a condition-type SECTIONAL-CONDITION
;; with a generalized accessor, SECTIONAL-CONDITION-SECTION
;; such that may be used in a condition class similar to each of
;;  ERRORS-DURING-FUNCTION-COMPILE and
;;  WARNINGS-DURING-FUNCTION-COMPILE

;; NB:
;; (compile nil '(lambda () unbound-symbol))
;; => #<FUNCTION (LAMBDA ()) {...}> T, T
;; ^ may serves to illustrate why COMPILE* and COMPILE** were each defined

;; NB: DNW for capturing the compile-time errors
#+NIL
(defun compile-lambda (defn)
  (handler-bind ((error #'(lambda (c)
                            (error "Caught ~S" c)))
                 (warning #'(lambda (c)
                              (warn "Caught ~S" c))))
    (compile nil defn)))
;; (compile-lambda '(lambda () unbound-symbol))
;; => #<FUNCTION (LAMBDA ()) {...}> T, T


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
  ;; See also: MK-LAMBDA prototype, common-lambda.lisp
  ;;           NB [Implementation-Specific Section]
  ;;
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

;; TBD: Deferred application of a local string-output-stream
;; for purpose of capturing compiler error and compiler warning
;; messages within any specific section of source eval.
;;
;; and/or
;;
;; TBD: Compile with localized handling for all error,
;;      and capture for warning messages

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
  ;; See also: MK-LAMBDA prototype, common-lambda.lisp
  ;;           NB [Implementation-Specific Section]
  ;;
  ;; FIXME: Align onto the error/waning handling semantics of COMPILE**
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
    ;; NB: This avoids any specific handling for any ERROR during the
    ;; actual COMPILE call, as a matter of ad hoc convenience
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



#+NIL ;; NB: DNW for capturing the compile-time errors
(defun compile-lambda (defn)
  (let ((*break-on-signals* '(or error warning)))
    (restart-case
        (compile nil defn)
      (continue (&rest what)
        ;; Unreached - Overriden by the implementation
        (format *debug-io* "Caught ~S" what)))))



#+NIL
(defun compile-lambda (defn)
  (let ((*break-on-signals* '(or warning error))
        (conditions))
    ;; The warning is presented when *BREAK-ON-SIGNALS* is bound as so,
    ;; but it's not clear as to how the condition resulting in the
    ;; "Break Condition" itself may be accessed, portably.
    (multiple-value-bind (fn warned-p errs-p)
        (restart-case
            (compile nil defn)
          (thunk ()
            ;; NB: This restart is defined only for purpose of the
            ;; following side-effect. It is clearly a hack.
            :test (lambda (c) (setf conditions (nconc conditions (list c)))
                          (values nil))
            (values nil)))
      (declare (ignore warned-p errs-p))
      ;; Note that the CONDITIONS value may not represent any actually
      ;; useful return value for any warned/failed call to COMPILE.
      ;;
      ;; This is one portable method available, for whatsoever accessing
      ;; the ERROR or WARNING conditions produced during COMPILE.
      ;;
      ;; It may be assumed that any binding onto *DEBUGGER-HOOK*
      ;; would produce a similar result - although more portably, rather
      ;; than by defininig an unreachable restart as above.
      ;;
      ;; See following annotations.
      (values fn conditions))))

;; NB: SBCL COMPILE calls SB-C:COMPILE-IN-LEXENV with a NULL LEXENV.
;;     As such, any condition handlers defined in the environment in
;;     which COMPILE is called will be unavaialble within that section
;;     of the call to COMPILE, itself.
;;
;; While portable per CLtL2 pedagogy, it may present a principally
;;     needless limitation to applications, at least in that single
;;     implementation.
;;
;; It should be possible to define a portable analogy to
;; COMPILE-IN-LEXENV (CMUCL, SBCL) such that would operate similar to
;; COMPILE -- and may relevant espcially for compiling anonymous
;; labmda functions -- that would accept an explicit ENV argument, not
;; in all ways dissimilar to GET-SETF-EXPANSION
;;
;; or should one simply call (coerce <form> 'function) ???

;; (coerce '(lambda () unbound-symbol)  'function)
;: ^ TBD: How many implementations there are in which that differs
;;        to the behaviors of COMPILE for an anonymous lambda form.

;; ^ AHAH (for SBCL)


;; TBD: Why there's ever a spurious NIL in the last element of the
;;      CONDITIONS list, as returned on any single implementation.

;; (compile-lambda '(lambda () unbound-symbol))

;; NB: The original condition is stored with the previous, but with a
;;     return value such that must be considered implementation-specific.
;;
;; (describe  (car (nth-value 1 (compile-lambda '(lambda () unbound-foo)))))
;;
;; ..
;; (simple-condition-format-arguments  (car (nth-value 1 (compile-lambda '(lambda () unbound-foo)))))
;; ^ the original condition is available there, in SBCL, but not with any
;;   portably meaningful representation.


;; NB: Concerning this methodology with *BREAK-ON-SIGNALS* and the
;; peculiar :TEST handler within RESTART-CASE, it may be considered a
;; novel methodology but not in itself useful.
;;
;; Simply calling COMPILE with *BREAK-ON-SIGNALS* similarly bound may be
;; sufficient, although it may not be in itself useful for programmed
;; reflection on any set of condition objects produced during COMPILE


#+NIL ;; ...
(defun compile-lambda (defn)
  (let ((*break-on-signals* '(or warning error))
        (conditions))
    (multiple-value-bind (fn warned-p errs-p)
        (restart-case
            (handler-bind ((condition (lambda (c)
                                        (format *debug-io* "~%Frob ~S" c))))
              ;; ^ DNW for capturing the conditions during COMPILE
              (compile nil defn))
          (thunk ()
            (values nil)))
      (declare (ignore warned-p errs-p))
      (values fn conditions))))

;; (compile-lambda '(lambda () unbound-foo))
