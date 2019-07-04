;; common-vec.lisp - utilities for Common Lisp vectors
;;-----------------------------------------------------------------------------
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
  ;; FIXME/DEPRECATED - See DO-MAPPED. common-seq.lisp
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
