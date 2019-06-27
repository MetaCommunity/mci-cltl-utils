;; mop-utils.lisp - utilities onto MOP implementations
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/common/mop)


(defmacro validate-class (class &optional (superclass 'standard-class))
  #+(or SBCL CMU CCL ALLEGRO)
  `(progn
     (defmethod validate-superclass ((a ,class) (b ,superclass))
       (values t))
     (defmethod validate-superclass ((a ,superclass) (b ,class))
       (values t)))
  #-(or SBCL CMU CCL ALLEGRO)
  `(progn
     (simple-style-warning
      "~<validate-class -~> ~<No known class/superclass validation ~
in this implementation -~> ~<class ~s with superclass ~s~>"
      (quote ,class) (quote ,superclass))
     (values)))


;; -- Other Utils

;; ---- Class Precedence Reflection

(eval-when (:compile-toplevel :load-toplevel :execute)

;; NB: ensure LEAST-COMMON-SUPERCLASS is defined in the environment,
;; previous to the INSTANCE deftype, such that uses MAKE-LOAD-FORM with
;; CONSTANTP non-nil.

(declaim (ftype (function (class-designator class-designator)
                          (values class cons &optional))
                least-common-superclass))

(defun least-common-superclass (a b)
  ;; NB: MOP-util Onto CLOS, per se
  (let* ((%a (compute-class a))
         (%b (compute-class b))
         (common (nreverse
                  (the cons
                       (intersection (the cons (class-precedence-list %a))
                                     (the cons (class-precedence-list %b))
                                     :test #'eq)))))
    (declare (type class %a %b)
             (type cons common))
    (values (car common) common)))


) ;; EVAL-WHEN

;; (least-common-superclass 'standard-object 'structure-object)
;; (least-common-superclass 'structure-object 'standard-object)


;; ---- Portable INSTANCE (not CONDITION) alias type

(deftype instance ()
  (mk-lf (class-name
          ;; FIXME - CONDITION is not ever TYPEP SLOT-OBJECT [SBCL]
          #+NIL
          (least-common-superclass
           (find-class 'condition)
           (least-common-superclass 'standard-object
                                    'structure-object))
          ;; ^ NB: The previous form returns the same as the following
          ;; form, in SBCL. The previous is not used, here, as it
          ;; introduces an inconsistency from the implementation.
          ;;
          ;; This inconsistency has been described to a further
          ;; extent, in the project tech note, `CONDITION` object is not
          ;; ever typep `SLOT-OBJECT` in some SBCL - avaialble in the
          ;; project  documentation files under doc/markdown/
          #-NIL
          (least-common-superclass 'standard-object
                                   'structure-object))))
