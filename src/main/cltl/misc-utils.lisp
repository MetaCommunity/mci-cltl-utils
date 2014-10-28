;; misc-utils.lisp - assorted utility forms

(in-package #:utils)


(defmacro defconstant* (name value &optional docstring)
  `(defconstant ,name
     (cond
       ((boundp (quote ,name))
	(symbol-value (quote ,name)))
       (t ,value))
     ,@(when docstring
	     (list docstring))))



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

