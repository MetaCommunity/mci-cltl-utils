;; seq-utils.lisp - utilities for Common Lisp sequences
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

;;; % List Utilities

(defmacro push-last (a l)
  ;; FIXME - Iterative application (with-list-append ?)
  ;; - Using a store variable STOR => (NIL)
  ;;   and a pointer variable PTR => (CDR STOR)
  ;;   such that the CDR of PTR is set to (CONS A)
  ;;   returning lastly (CDR STOR)
  ;;   this should not need to use LAST
  (with-symbols (%l)
    `(let ((,%l ,l))
       (cond
	 ((consp ,%l)
	  (rplacd (last ,%l) (list ,a))
	  ,%l)
	 (t (setf ,l (list ,a)))))))


#-(and)
(eval-when ()

  (push-last 3 '(1 2))
  ;; => (1 2 3)

  (let ((l '(1 2)))
    (eq (push-last 3 l) l))
  ;; => T

  (let ((l nil))
    (values (eq (push-last 3 l) l) l))
  ;; => T, (3)

  (let ((l (list nil)))
    (push-last 'a l)
    (values (push-last 'b l)
            l))
  ;; => (NIL A B), (NIL A B)

  )




;;; % Vector Utilities

(defun simplify-vector (vector)
  (declare (type vector vector)
	   (values simple-array))
  (coerce vector (list 'simple-array (array-element-type vector)
		       (list (length vector)))))

#-(and)
(eval-when ()
  (let* ((v-in (make-array 1 :fill-pointer 1
                            :element-type 'fixnum
                            :initial-element 0))
         (v-out (simplify-vector v-in)))
    (values (array-element-type v-out)
            (= (length v-in) (length v-out))
            (array-has-fill-pointer-p v-out)
            v-out))
  ;; => FIXNUM, T, NIL, #(0)
  )


(defmacro do-vector ((elt v &optional return) &body body)
  (with-symbols (%v len n)
    `(let* ((,%v ,v)
	    (,len (length ,%v)))
       (declare (type vector ,%v)
		(type array-length ,len))
       (dotimes (,n ,len ,return)
	 (declare (type array-dim ,n))
	 (let ((,elt (aref ,%v ,n)))
	   ,@body)))))


#-(and)
(eval-when ()

  (let ((buff (list nil)))
    (do-vector (c "FOO" (cdr buff))
      (push-last c buff)))
  ;; => (#\F #\O #\O)

  (let ((buff nil))
    (do-vector (c "FOO" buff)
      (push-last c buff)))
  ;; => (#\F #\O #\O)

  (let (buff)
    (do-vector (c "FOO" buff)
      (push c buff)))
  ;; => (#\O #\O #\F)
  )

;;; %% String Utilities

(defun simplify-string (string)
  "If STRING can be coerced to a SIMPLE-BASE-STRING, then return a
SIMPLE-BASE-STRING representative of the contents of STRING.
Otherwise, return a SIMPLE-STRING representative of the
contents of STRING."
  (declare (type string string)
	   (inline coerce)
	   (values (or simple-string simple-base-string)))
  (handler-case
      (coerce string 'simple-base-string)
    (type-error ()
      (coerce string 'simple-string))))

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

(defmacro string-position (char str &body rest)
  ;; This macro expands to a strongly-typed wrapper for CL:POSITION
  ;;
  ;; a type-dispatching form, towards applying compiler optimizations
  ;; at runtime, cf. SB-KERNEL:%FIND-POSITION, and no-doubt similar in CMUCL

  ;; NB SBCL emits 'deleting unreachable code' when compiling this
  ;; macroexpansion within SPLIT-STRING-1. The compiler may be inferring
  ;; that the default (t) form may be unreachable, there.

  ;; FIXME/TD - Portable type system reflection for Lisp compiler
  ;; environments, pursuant towards portable compiler macro definition
  ;; - When the type of STR can be inferred, use static dispatching for
  ;;   that type - this could entail some dynamic dispatching, as when
  ;;   the type of STR would denote a union type
  ;; - When not, use dynamic dispatching - entailing, e.g that the
  ;;   default 't' form will be compiled
  ;;
  ;; - Consider developing a CLOS-like interfce for the compiler
  ;;   environment, but such that must allow method specialization at a
  ;;   finer granularity than Common Lisp classes. See also, the
  ;;   implementation type system in each of CMUCL, SBCL, CCL, ....
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

(defun null-string ()
  (declare (values simple-base-string))
    ;; FIXME declare inline
  (values +null-string+))

(defun string-null-p (str)
  (declare (type string str)
	   (values boolean))
  ;; FIXME: declare inline
  (or (eq str +null-string+)
      ;; FIXME_DOCS note opportunities for object reuse in ANSI CL
      ;; programs, and correspondingly, opportunities for using EQ as
      ;; an equivalence test
      (and (typep str 'string)
	   (zerop (length (the string str))))))


(deftype array-dim ()
  ;; FIXME_DOCS See also `array-length'
  '(integer 0 (#.array-dimension-limit)))

(deftype array-length ()
  ;; FIXME_DOCS See also `array-dim'
    '(integer 0 #.array-dimension-limit))

(defun split-string-1 (char str &key (start 0) end from-end
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
		   (or array-dim null)))
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

(defun split-string (char str &key (start 0)  end
				from-end  key
				(test #'char=)
				test-not)
  ;; FIXME_DOCS See also `split-string-1'
  (declare (type string str)
	   (values list))
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
