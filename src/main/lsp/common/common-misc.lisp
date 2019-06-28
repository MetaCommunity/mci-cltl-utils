;; common-misc.lisp - lisp compiler integration (misc)
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


(declaim (ftype (function ((or symbol cons))
                          (values boolean (or symbol cons) (or symbol cons)
                                  &optional))
                featurep))

(defun featurep (expr)
  ;;
  ;; NB: This function does not provide support for *FEATURES* elements
  ;; interned in any CL:PACKAGE not equivalent to either *PACKAGE* or
  ;; the :KEYWORD package
  ;;
  ;; --
  ;;
  ;; first return value:
  ;;  a boolean indicating whether the EXPR is interpreted as
  ;;  representing a true feature expression
  ;;
  ;; second return value:
  ;;  an inner-most element within EXPR that did not evaluate as a true
  ;;  feature expresion
  ;;
  ;; third return value
  ;;  an inner-most element within EXPR that evaluated as a true feature
  ;;  expression
  ;;
  ;;
  ;; NB: Return values of this function should not be destructively
  ;;     modified, as they may represent constant data.
  ;;
  ;; --
  ;;
  ;; The second and third return values are provided in an interest of
  ;; systems debugging, for nested feature expressions
  ;;
  (let (%mangled-name%)
    (declare (type symbol %mangled-name%))
    (labels ((mangle-name-kwd (name)
               (setq %mangled-name%
                     (intern (symbol-name name)
                             (load-time-value (find-package :keyword)
                                              t))))
             (feature-declared-p (name)
               ;; FIXME: Implementation of a name mangling function
               ;; providing behaviors similar to CLtL2 reader macros #\#\+
               ;; and #\#\-
               ;;  - would have to be implemented onto I/O streams
               ;;  - may serve to support *FEATURES* expressions not
               ;;    interned in the #:KEYWORD package
               ;;  - is not provided here
               (cond
                 ((find name *features* :test #'eq)
                  ;; NB: The first check for any symbol expr is
                  ;; performed, in this function, using that symbol's
                  ;; initial SYMBOL-PACKAGE
                  (values t nil name))
                 ((find (mangle-name-kwd name) *features* :test #'eq)
                  ;; Subsequently, check for a symbol with equivalent
                  ;; SYMBOL-NAME, in the :KEYWORD package.
                  ;;
                  ;; This labels function returns the keyword symbol, if found.
                  ;;
                  ;; NB: That symbol might be - in effect - lost for
                  ;; some purposes of top-level return value capture, in
                  ;; the methodology of return-value capture for feature
                  ;; expressions, as developed internally in this function.
                  ;; Regardless, it should be visible to some (TRACE FEATUREP)
                  ;;
                  (values t nil %mangled-name%))
                 (t (values nil name nil))))
             (and-declared-p (exprs)
               (dolist (expr (cdr exprs) (values t nil exprs))
                 (multiple-value-bind (decl-p fail pass)
                     (featurep expr)
                   (declare (ignore pass))
                   (unless decl-p
                     (return (values nil fail nil))))))
             (or-declared-p (exprs)
               (dolist (expr (cdr exprs) (values nil exprs nil))
                 (multiple-value-bind (decl-p fail pass)
                     (featurep expr)
                   (declare (ignore fail))
                   (when decl-p
                     (return (values t nil pass)))))))
      (etypecase expr
        (symbol (feature-declared-p expr))
        (cons
         (let ((frst (car expr)))
           (ecase frst
             (and (and-declared-p expr))
             (or (or-declared-p expr))
             (not (multiple-value-bind (decl-p fail pass)
                      (feature-declared-p (cadr expr))
                    (declare (ignore fail pass))
                    (cond
                      (decl-p (values nil expr nil))
                      (t (values t nil expr))))))))))))

;; FIXME: Note the topic, Global Locking During *FEATURES* Eval,
;; in project doc/markdown/ src. FEAUTREP should not be assumed
;; to be safe for application during concurrent evaluation of arbitrary
;; source forms such that may modify the value of *FEATURES*


;; (featurep 'sbcl)
;; =[SBCL]=> T, NIL, :SBCL

;; NB - by side effect
;; (featurep '#:sbcl)
;; =[SBCL]=> T, NIL, :SBCL

;; (featurep '(not sb-thread))

;; (featurep '(and sbcl sb-thread))

;; (featurep '(and openmcl cmucl))

;; (featurep '(and :sbcl :sb-thread))

;; (featurep '(and :sbcl (not :sb-thread)))

;; (featurep '(and :ansi-cl (or :unsupp-1 :unsupp-2)))

;; (featurep '(and :ansi-cl (and (not :unix) :unsupp-2)))

;; (featurep '(and :ansi-cl (or (not :unix) :unsupp-2)))

;; (featurep '(and :ansi-cl (or :unsupp-2 :unix)))

;; (featurep '(and :ansi-cl (and :unsupp-2 :unix)))

;; --

(defmacro mk-lf (form)
  "Evaluate FORM within a constantp LOAD-TIME-VALUE expression"
  `(load-time-value ,form t))

