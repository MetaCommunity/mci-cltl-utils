;; fdef-acc.lisp - Accessor Fundtion Definition for ltp-stor-fdef system
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

(in-package #:ltp/stor/fdef)

#+NIL
(deftype subtype (name)
  (labels ((locally-check-one-subtype (spec)
             (subtypep spec name)))
    ;; DNW - the type expansion may not be storing the lexical
    ;; environment in which it was defined
    `(satisfies locally-check-one-subtype)))

;; (typep '(find-class 'simple-base-string) '(subtype simple-string))

#+TBD
(deftype finalized-class (&optional (name t))
  ;; The ,NAME spec below DNW except for the class named T.
  ;;
  ;; In order to provide a (SATISTFIES SUBTYPE-OF-<TYPE>) predicate
  ;; for an arbitrary NAME, this DEFTYPE could use a methodology for
  ;; defininig arbitrary functions for purposes of subtype eval, e.g
  ;; SUBTYPE-OF-STRING.
  ;;
  ;; However, such a methodology could be thought to comprise a manner
  ;; of a memory leak.
  `(and class ,name (satisfies class-finalized-p)))

;; (typep (find-class 'string) 'finalized-class)
;; => T

;; (typep (find-class 'simple-base-string) '(finalized-class simple-string))
;; ^ DNW

(deftype finalized-class ()
  `(and class (satisfies class-finalized-p)))

(define-condition unfinalized-class (type-error)
  ;; FIXME - move into a base-class shared system; reuse in LTP singleton
  ()
  (:default-initargs :expected-type 'finalized-class)
  (:report
   (lambda (c s)
     (format s "~<Not a finalized class:~>~< ~S~>"
             (type-error-datum c)))))


(declaim (inline ensure-class-finalized-p))

(defun* ensure-class-finalized-p (class)
  ;; FIXME: Move this source definition and the corresponding type and
  ;; condition class definitions, above, into ltp/common/mop src
  ;;
  ;; NB: See also, alternate definition with annotations
  ;; ltp-main:src/test/lsp/with-condition-restart.lisp
  ;;
  (declare (type class-designator class)
           (values class &optional))
  (let ((%class (compute-class class)))
    (declare (type class %class))
    (restart-case
        (cond
          ((class-finalized-p %class)
           (values %class))
          (t
           (error 'unfinalized-class
                  :expecteted-type 'finalized-class
                  :datum %class)))
      (finalize ()
        ;; ASSUMPTION: That this restart may be reached only as a result
        ;; of the failed CLASS-FINALIZED-P condition, denoted above,
        ;; rather than by any form resulting from CLASS-FINALIZED-P
        :report "Finalize Class"
        (finalize-inheritance %class)
        (values %class)))))

;; (ensure-class-finalized-p 'string)

;; (class-finalized-p (ensure-class-finalized-p (defclass #.(make-symbol "frob") () ())))
;; should -> T once the FINALIZE restart is invoked

;; NB: An extension onto STANDARD-CLASS may be defined as to ensure that
;; any class is finalized before it may be subsequently used, pursuant
;; of a class' defintion as other than a forward-referenced class.
;;
;; Some extensions onto STANDARD-CLASS moreover may be defined as to
;; require that a class will not be redefined once it is finalizeed.
;;
;; In such extensions, ENSURE-CLASS-FINALIZED may be -- in effect --
;; redundant.
;;
;; Regardless, perhaps the following source forms may be applied about
;; non-finalized classes - an error condition, such that this program
;; will endeavor to signal as such.

;; ----------

(defgeneric compute-accessor-ftype (slotdef class))

#+TBD
(defun* compute-direct-accessor-ftypes (class)
  (declare (type class class)
           (values list &optional))

  (%ensure-class-finalized-p class)
  )

#+TBD
(defun define-direct-accessors (class)
  ;; TBD: Folding DEFUN calls into method forms, rather than expanding
  ;; to DEFUN source forms within a macroexpansion
)

  (let ((class-at-now ;; FIXME
         ))
  (with-symbols (%class %%class)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,%class (compute-class (quote ,class))))





         )))))


