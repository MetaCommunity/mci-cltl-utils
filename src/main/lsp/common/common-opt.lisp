;; common-opt.lisp - onto compiler optimizations
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


(defmacro with-optimization ((&rest policy) &body body)
  ;; NB: POLICY will not be evaluated. See also: PROCLAIM
  ;;
  ;; Usage: WITH-TAIL-RECURSION defn
  `(locally (declare (optimize ,@policy))
     ,@body))

;; studies in implementation-specific optimizations
;; see also:
;;  http://0branch.com/notes/tco-cl.html
;;  http://trac.clozure.com/ccl/wiki/DeclareOptimize
;;  http://ecls.sourceforge.net/new-manual/ch02.html#ansi.declarations.optimize

(defmacro with-tail-recursion (&body body)
  ;; used in SPLIT-STRING definition
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

