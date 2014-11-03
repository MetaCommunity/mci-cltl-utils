;; opt-utils.lisp - selected compiler optimizations (prototype)

(in-package #:utils)

(defmacro with-optimization ((&rest policy) &body body)
  `(locally (declare (optimize ,@policy))
     ,@body))

;; studies in implementation-specific optimizations
;; see also:
;;  http://0branch.com/notes/tco-cl.html
;;  http://trac.clozure.com/ccl/wiki/DeclareOptimize
;; ...
;; well formatted tables:
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
		(ccl::policy.allow-tail-recursion-elimination env))))
  (values (test-tail  (debug 2) (speed 3) (safety 2) (space 2))
	  (test-tail  (debug 3) (speed 0) (safety 3) (space 0))))
;; #+CCL => #<CCL::LEXICAL-ENVIRONMENT ...> , ...
