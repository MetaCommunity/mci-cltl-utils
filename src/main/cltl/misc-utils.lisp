;; misc-utils.lisp - assorted utility forms

(in-package #:utils)


(defmacro defconstant* (name value &optional docstring
                                     &environment env)
  "Define NAME as a constant variable with value VALUE and optional
DOCSTRING

If NAME denotes an existing variable and its value is EQUALP
to the specified VALUE, then the value previously bound to NAME will
be reused. Otheriwse, the new VALUE will be used for defining the
constant variable NAME. This differs from the behavior of
`CL:DEFCONSTANT' in that EQUALP is applied as the comparison, rather
than EQL, and that the previous value is reused when possible.

If NAME does not denote an existing variable, then this macro's
behavior is equivalent to `CL:DEFCONSTANT'"
  (with-gensym (%value %previous)
    (let ((%name (nth-value 4  (get-setf-expansion name env))))
      ;; Ed. note: SBCL 1.2.5 was not handling a simpler form
      ;; when a certain file in McCLIM was compiled and loaded. So,
      ;; rather than using SYMBOL-VALUE directly on the NAME, this
      ;; will now try to wrap the reference to the symbol-value of
      ;; NAME around a return value from GET-SETF-EXPANSION w/ ENV
      ;; ...and still it doesn't work out. Effectively, it must be
      ;; that the BOUNDP call returns true (how?) but the SYMBOL-VALUE
      ;; call fails.
      ;;
      ;; So, EVAL instead of SYMBOL-VALUE ? Still, "Doesn't work out".
      ;; The 1th item in the backtrace is a FOP-FUNCALL, moreover,
      ;; preventing a complete debug of this particular issue.
      ;;
      ;; affected forms (McCLIM MCi fork)
      ;; * AUTOMATON::+MIN-CHAR-CODE+ (Drei state-and-transition.lisp) 
      ;; * AUTOMATON::+INTERSECTION+ and later constants (Drei regexp.lisp)
      ;; 
      ;; Each of those symbols is bound to FIXNUM.
      ;; 
      ;; Workaround: Use DEFCONSTANT instead, in those
      ;; bindings, considering: A FIXNUM is always EQL to itself
      ;;
      ;; Issue encountered in:
      ;;  * SBCL 1.2.5 Linux 64 bit
      ;;  * SBCL "1.2.5.76-65a44db" Linux 64 bit
      `(defconstant ,name 
         (cond
           ((boundp (quote ,%name))
            (let ((,%previous (symbol-value (quote ,%name)))
                  (,%value ,value))
              (cond
                ((equalp ,%value ,%previous) ,%previous)
                ;; pass through for debugger
                (t ,%value))))
           (t ,value))
         ,@(when docstring
                 (list docstring))))))

;; (defconstant* foo 'foo)


;;; % COMPILE*

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



(defun compile* (name &optional defn)
  "Evalute COMPILE on NAME and DEFN. 

If COMPILE indicates errors during compilation, an error of type
`ERRORS-DURING-COMPILE' is signaled.

If COMPILE indicate warnings during compilation, a warning of type
`WARNINGS-DURING-COMPILE` is signaled, and the compiled function is
returned.

In other instances, the compiled function is returned."
  (declare (type function-designator name)
	   (values function))
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


(defun symbol-status (s)
  (declare (type symbol s)
	   (values symbol (or package null)))
  (let ((pkg (symbol-package s)))
    (cond
      (pkg
       (multiple-value-bind (s status)
	   (find-symbol (symbol-name s)
			pkg)
	 (declare (ignore s))
	 (values status pkg)))
      (t (values nil nil)))))

;; (symbol-status 's)
;; => :INTERNAL, #<PACKAGE "INFO.METACOMMUNITY.CLTL.UTILS">
	
;; (symbol-status 'print)
;; => :EXTERNAL, #<PACKAGE "COMMON-LISP">

;; (symbol-status 'utils::print)
;; => :EXTERNAL, #<PACKAGE "COMMON-LISP">

;; (symbol-status '#:foo)
;; => NIL, NIL

