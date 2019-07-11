;; common-list.lisp - utilities for Common Lisp lists
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


(defmacro map-plist (fn whence)
  (with-symbols (retv dispatch p v %whence %%whence %fn val)
    `(let ((,%fn ,fn)
           (,%%whence ,whence))
       (labels ((,dispatch (,%whence ,val)
                  (cond
                    ((and (consp ,%whence)
                          (consp (cdr ,%whence)))
                     (let* ((,p (car ,%whence))
                            (,v (cadr ,%whence))
                            (,retv (funcall ,%fn ,p ,v)))
                       (,dispatch (cddr ,%whence)
                                  (nappend ,val (list ,retv)))))
                    (,%whence
                     (error "~<Invalid property list syntax:~>~< ~S~>"
                            ,%%whence))
                    (t (values ,val)))))
         (,dispatch ,%%whence nil)))))

;; (map-plist #'cons '(:a 1 :b 2))
;; (map-plist #'cons '(:a (1) :b (2 3)))
;; (apply #'nconc (map-plist #'list '(:a 1 :b 2)))
;; (apply #'nconc (map-plist #'list '(:a (1) :b (2 3))))
;;
;; (map-plist #'cons '(:a 1 :b 2 :err))
;; (map-plist #'cons nil)

;; ----


(defmacro do-cons ((first rest whence &optional (return '(values))) &body body)
  (with-symbols (dispatch %whence)
    `(block nil
       (labels ((,dispatch (,%whence)
                  ;; NB: RETURN is referenced twice, below.
                  ;;
                  ;; The expanded value of RETURN will only be evaluated
                  ;; once.
                  (cond
                    ((consp ,%whence)
                     (let ((,first (car ,%whence))
                           (,rest (cdr ,%whence)))
                       ,@body
                       (cond
                         ((consp ,rest) (,dispatch ,rest))
                         (t (return ,return)))))
                    ((null ,%whence) (return ,return))
                    (t (error 'type-error
                              :expected-type 'list
                              :datum ,%whence
                              #+SBCL :context #+SBCL "as provided to DO-CONS"))
                    )))
         (,dispatch ,whence)))))

#+NIL
(eval-when ()

  (do-cons (a rest (ltp/common/mop:class-precedence-list (find-class 'string)))
    (format t "~%~S" a))

  (do-cons (a rest '(a b . c))
    (format t "~%~S , ~S" a rest))

  (do-cons (a b '(a . b))
    (format t "~%~S , ~S" a b))

  (do-cons (a b '(a))
    (format t "~%~S : ~S" a b))

  (do-cons (a b nil)
    (format t "~%~S : ~S" a b))

  (do-cons (a b 'arbitrary)
    (format t "~%~S : ~S" a b))

)
