;; common-seq.lisp - utilities for Common Lisp sequences
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

;;; % List Utilities

(defmacro push-last (a l)
  ;; FIXME - Iterative application (with-list-append ?)
  ;; - Using a store variable STOR => (NIL)
  ;;   and a pointer variable PTR => (CDR STOR)
  ;;   such that the CDR of PTR is set to (CONS A)
  ;;   returning lastly (CDR STOR)
  ;;   this should not need to use LAST

  ;; See also: NAPPEND, NPUSHL
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


(define-modify-macro nappend (&rest lists)
  nconc "Destructively modify LISTS via NCONC")

;; (let (a (b '(1 2))) (values (nappend a b) a b))

(defmacro npushl (value where)
  "Destructively modify WHERE such that a list with VALUE as its single
element becomes the LAST element of WHERE"
  `(setf ,where (nconc ,where (list ,value))))

;; (let (a) (values (copy-list a) (npushl 1 a) a))

;; (let ((b '(1 5))) (values (copy-list b) (npushl 17 b) b))


;;; % Vector Utilities

(defun* simplify-vector (vector)
  (declare (type vector vector)
	   (values (simple-array * (*)) &optional))
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


;; TBD: Define a compiler macro for SIMPLIFY-VECTOR such that - via
;; portable functions - would determine the element type and length of
;; the VECTOR, subsequently expanding into a form using those as
;; respectively static (when possible) and otherwise dynamic values.


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


;; see also: common-string.lisp
