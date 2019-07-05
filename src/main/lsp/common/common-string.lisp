;; common-string.lisp - string utilities for Common Lisp programs
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


(defun* simplify-string (string)
  "If STRING can be coerced to a SIMPLE-BASE-STRING, then return a
SIMPLE-BASE-STRING representative of the contents of STRING.
Otherwise, return a SIMPLE-STRING representative of the
contents of STRING."
  (declare (type string string)
	   (inline coerce)
	   (values (or simple-string simple-base-string)
                   &optional))
  (handler-case
      (coerce string 'simple-base-string)
    (type-error (c)
      (cond
        ((eq (type-error-datum c) string)
         (coerce string 'simple-string))
        (t (error c))))))

#-(and)
(eval-when ()
  ;; NB - SBCL
  (typep "FOO" 'simple-base-string)
  ;; => NIL

  (typep "FOO" 'simple-string)
  ;; => T

  (typep (simplify-string "FOO") 'simple-base-string)
  ;; => T

  (typep (simplify-string (make-string 0  :element-type 'base-char))
         'simple-base-string)
  ;; => T
)

(defmacro string-position (char str &rest rest)
  ;; This macro expands to a strongly-typed wrapper for CL:POSITION
  ;;
  ;; a type-dispatching form, towards applying compiler optimizations
  ;; at runtime, cf. SB-KERNEL:%FIND-POSITION, and no-doubt similar in CMUCL

  ;; NB SBCL emits 'deleting unreachable code' when compiling this
  ;; macroexpansion within SPLIT-STRING-1. The compiler may be inferring
  ;; that the default (t) form may be unreachable, there.

  ;; FIXME/TD - Portable type system reflection for Lisp compiler
  ;; environments, pursuant towards portable compiler macro definition
  ;;
  ;; - When the type of STR can be inferred, use static dispatching for
  ;;   that type - this could entail some dynamic dispatching, as when
  ;;   the type of STR would denote a union type
  ;;
  ;; - When not, use dynamic dispatching - entailing, e.g that the
  ;;   default 't' form will be compiled
  ;;
  ;; - Consider developing a CLOS-like interfce for the compiler
  ;;   environment, but such that must allow method specialization at a
  ;;   finer granularity than Common Lisp classes. See also, the
  ;;   implementation type system in each of CMUCL, SBCL, CCL, ....
  ;;
  ;; - Note also, compiler mcros
  (with-symbols (%char %str)
    `(let ((,%char ,char)
	   (,%str ,str))
       (declare (inline position))
       (typecase ,%str
	 (simple-base-string (position ,%char (the simple-base-string ,%str)
                                       ,@rest))
	 (simple-string (position ,%char (the simple-string ,%str)
				  ,@rest))
	 (string (position ,%char (the string ,%str) ,@rest))
	 (t (position ,%char (coerce ,%str 'simple-string)
                      ,@rest))))))


(defconstant* +null-string+
    (make-string 0 :element-type 'base-char))

;; (declare (inline null-string string-null-p))

(defun* null-string ()
  (declare (values simple-base-string &optional))
    ;; FIXME declare inline
  (values +null-string+))

(defun* string-null-p (str)
  (declare (type string str)
	   (values boolean &optional))
  ;; FIXME: declare inline
  (or (eq str +null-string+)
      ;; FIXME_DOCS note opportunities for object reuse in ANSI CL
      ;; programs, and correspondingly, opportunities for using EQ as
      ;; an equivalence test
      (and (typep str 'string)
	   (zerop (length (the string str))))))


(defun* split-string-1 (char str &key (start 0) end from-end
                             key (test #'char=) test-not )
  ;; FIXME_DOCS See also `split-string'
  ;; FIXME_DOCS note use of CL:SIMPLE-STRING in the type signature for
  ;; the return values of this function
  ;;
  ;; Concerning a "deleting unreachable code" message from SBCL when
  ;; compiling this function, see commentary in STRING-POSITION src.
  (declare (type string str)
	   (values simple-string
		   (or simple-string null)
		   (or array-dim null)
                   &optional))
  (let ((n (string-position char str
	     :start start :end end
	     :from-end from-end :key key
	     :test test :test-not test-not)))
    (cond
      ((and n (zerop n))
       (values +null-string+
	       (subseq str 1)
	       0))
      (n (values (subseq str 0 n)
		 (subseq str (1+ n))
		 n))
      (t (values str nil nil)))))

;; (split-string-1 #\. (simplify-string "a.b"))
;; => "a", "b", 1
;; (split-string-1 #\. (simplify-string ".b"))
;; => "", "b", 0
;; (split-string-1 #\. (simplify-string "a."))
;; => "a", "", 1
;; (split-string-1 #\. (simplify-string "ab"))
;; => "ab", NIL, NIL
;;
;; (split-string-1 #\. (simplify-string "a.b.c") :start 2)
;; => "a.b", "c", 3
;; (split-string-1 #\. "a.b.c" :start 2)
;; => "a.b", "c", 3


(defun split-string (char str &key (start 0)  end
                           from-end  key
                           (test #'char=)
                           test-not)
  ;; FIXME_DOCS See also `split-string-1'
  (declare (type string str)
	   (values list &optional))
  (with-tail-recursion ;; FIXME_DOCS note usage case here
    (labels ((split (str2 buffer)
	       (declare (type string str)
			(type list buffer))
	       (multiple-value-bind (start rest n)
		   (split-string-1 char str2
				   :start start :end end
				   :from-end from-end :key key
				   :test test :test-not test-not)
		 (cond
		   (n
                    ;; FIXME_DESIGN cost of `push-last' onto arbitrary lists?
		    (let ((more (push-last start buffer)))
		      (split rest more)))
		   (t (push-last str2 buffer))))))
      (split str nil))))

;; (split-string #\. (simplify-string "a.b.c.d"))
;; => ("a" "b" "c" "d")

;; (split-string #\. (simplify-string ".b.c.d"))
;; => "" "b" "c" "d")

;; (split-string #\. (simplify-string "abcd"))
;; => ("abcd")

;; (split-string #\. (simplify-string "..."))
;; => ("" "" "" "")
