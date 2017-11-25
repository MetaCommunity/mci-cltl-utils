;; opt-utils.lisp - onto compiler optimizations
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

(in-package #:ltp-utils)

(defmacro with-optimization ((&rest policy) &body body)
  `(locally (declare (optimize ,@policy))
     ,@body))

;; studies in implementation-specific optimizations
;; see also:
;;  http://0branch.com/notes/tco-cl.html
;;  http://trac.clozure.com/ccl/wiki/DeclareOptimize
;;  http://ecls.sourceforge.net/new-manual/ch02.html#ansi.declarations.optimize

(defmacro with-tail-recursion (&body body)
  `(with-optimization (#-allegro (debug 2) 
				 #+allegro (debug 1)
				 (speed 3) (safety 2))
     ,@body))



#+NIL
(macrolet ((with-opt-env ((env &rest opt) &body body &environment environment)
	     `(let ((,env ,environment))
		(declare  (optimize ,@opt))
		,@body))
	   (test-tail (&rest opt)
	     `(with-opt-env (env ,@opt)
		#+CCL 
		(ccl::policy.allow-tail-recursion-elimination env)
                ;; FIXME implement test form here
                )))
  (values (test-tail  (debug 2) (speed 3) (safety 2) (space 2))
	  (test-tail  (debug 3) (speed 0) (safety 3) (space 0))))
;; #+CCL => #<CCL::LEXICAL-ENVIRONMENT ...> , ...

(define-condition compilation-condition ()
  ())

(define-condition compilation-warning  (warning compilation-condition)
  ())

(define-condition simple-compilation-warning (simple-condition compilation-warning)
  ())

(define-condition compilation-error (error compilation-condition)
  ())

(define-condition simple-compilation-error (simple-condition compilation-error)
  ())


(defmacro compile* (form &optional optimization)
  (with-gensym (%form fn warnedp failurep)
    `(with-optimization (,@optimization)
       (let ((,%form ,form))
         (multiple-value-bind  (,fn ,warnedp ,failurep)
             (compile nil ,%form)
           (when ,warnedp
             (warn 'simple-compilation-warning
                   :format-control
                   "~<Compiler warned while compiling ~S~>~
~@[ ~<[using optimization ~S]~>~]" 
                   :format-arguments (list ,%form (quote ,optimization))))
           (cond
             (,failurep 
              (cerror  (format nil "Continue, returning ~s"  ,fn)
                       'simple-compilation-error
                       :format-control
                       "~<Compiler erred while compiling ~S~>~
~@[ ~<[using optimization ~S]~>~]" 
                       :format-arguments (list ,%form (quote ,optimization)))
              (values ,fn))
             (t (values ,fn))))))))


;; (compile* '(lambda () (foo bar)) (debug 3))

;; (compile* '(lambda () (cl:print)) (debug 3))

;; (compile* '(lambda () (foo bar)))

;; (compile* '(lambda () (+ 1 a)))
