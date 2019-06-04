;; fn-utils.lisp
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

;; TD: DEFMACRO DEFUN* => (progn (declaim (ftype ...)) (defun ...))

(deftype setf-function-designator ()
  '(cons (eql setf) (cons symbol null)))

(deftype lambda-function-designator ()
  '(cons (eql lambda) (cons list t)))

(deftype function-designator ()
  ;; NB: Using TYPEP as an argument parser/assert for CONS subtypes
  '(or symbol function
    setf-function-designator
    lambda-function-designator))

(defun* compute-function (form)
"Evaluate FORM as a function designator, returning the function
designated by FORM.

Valid Syntax of FORM:
* <SYMBOL> denoting a function name
* (SETF <SYMBOL>) denoting a funtional SETF name
* (LAMBDA () <BODY>) as a list, denoting an anonymous function
* <FUNCTION> denoting itself

If FORM is a LAMBDA form, this function will return the functional
representation of FORM.

See also: COMPILE*, COMPILE**"
  (declare (type function-designator form)
           (values function &optional))
  (etypecase form
    (symbol (values (fdefinition form)))
    (function (values form))
    (cons
     (let ((type (car form)))
       (ecase type
         (setf
          (values (fdefinition form)))
         (lambda
          (values (coerce form 'function))))))))

;;;; Instance Tests
;;
;; (compute-function 'print)
;; (compute-function (quote (lambda () (list 1 2 3))))
;; (compute-function '(setf gethash))
;; (compute-function #'function)
;;
;; (compute-function (quote (lambda () (funcall (make-symbol "%fail")))))
;; (compute-function `(lambda ,(values)))
;;
;;;; Instance "Fail Tests"
;;
;; (compute-function (gentemp))
;; (compute-function (list 'setf (gentemp)))
;; (compute-function (list 'unknown 1 2 3))
;; (compute-function (quote (lambda syntax-error)))


(defmacro with-safe-frefs (specs &body body)
  ;;; NB: Prototype of a utility that could be useful for PERFORM
  ;;; methods within ASDF system definitions - in effect for making a
  ;;; forward reference to a function, without trying to intern a symbol
  ;;; in a package as yet undefined.

  "Evaluate BODY in a lexiccal environment in which each element of SPECS
denotes a function.

Syntax:
>    WITH-SAFE-FREFS ({SPECIFIER}*) {DECLARATION} {FORM}*
>
>    SPECIFIER: A _safe function reference_

>    DECLARATION, FORM: Like as in [CLHS]

A _safe function reference_ denotes a set of a function name, with an
optional pacakge specifier, and a symbol to which the function denoted by
the function name is to be bound when the FORMS are evaluated.

Example:

    (with-safe-frefs
        ((l #:list #:cl)
         (c #:identity))
      (funcall c (funcall l 2)))
    => (2)"
  ;; FIXME N/A for (SETF FOO) function names
  (let ((%s (gentemp "%s-"))
	(%foundp (gentemp "%foundp-")))
    (flet ((s-name (s)
	     (etypecase s
	       (string s)
	       (symbol (symbol-name s))
	       (character (string s))))
	   (pkg-name (p)
	     (etypecase p
	       (package (package-name p))
	       (symbol (symbol-name p))
	       ((or string character)
		(values p)))))
      `(let (,@(mapcar (lambda (spec)
			 (destructuring-bind
			       (name fn &optional
				     (package
				      (etypecase fn
					(symbol
					 (or (symbol-package fn)
					     *package*))
					((or string character)
					 *package*))))
			     spec
			   (let ((fname (s-name fn))
				 (pkgname (pkg-name package)))
			     `(,name
				(multiple-value-bind (,%s ,%foundp)
				    (find-symbol ,fname
						 ,pkgname)
				  (cond
				    (,%foundp
				     (values (fdefinition ,%s)))
				    (t
				     (error "Package ~A does not contain a symbol ~S"
					    (find-package ,pkgname)
					    (quote (quote ,fname))))
				    ))))))
		       specs))
	 ,@body))))


;; (macroexpand-1 '(with-safe-frefs ((l list)) (funcall l 1 2)))
;; (with-safe-frefs ((l #:list #:cl)) (funcall l 1 2))
;;; => (1 2)

#+NIL ;; Instance Test
(macroexpand (quote
(with-safe-frefs
    ((l #:list #:cl)
     (c #:compute-function))
  (funcall l (funcall c 'expt) 2))
))

(defun* function-name (fn)
  (declare (type function-designator fn)
           (values function-designator))
  (multiple-value-bind (lambda closure-p name)
           (function-lambda-expression (compute-function fn))
    (declare (ignore lambda closure-p))
    (values name)))

;; (function-name #'(setf gethash))
;; => (SETF GETHASH)

;; (function-name #'print)
;; => PRINT

;; (function-name (lambda () (+ 1 1 )))
;; => (LAMBDA ())
