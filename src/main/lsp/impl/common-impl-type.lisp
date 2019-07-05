;; common-impl-type.lisp - Implementation Support for Type Sys. Reflection, SBCL
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


(in-package #:ltp/common/mop)

#+TBD
(defsignature defun type-defined-p (spec)
              (:parameters type-designator)
              (:values boolean &optional)
              (:documentation
               "Return a boolean value indicating whether the expression
SPEC denotes a type description, as per all named and derived type
definitions and class definitions in the null lexical environment"))

#+SBCL
(defun type-defined-p (spec)
  (declare (type type-designator)
           (values boolean &optional))
  ;; NB: Operates on the null lexical environment, intrinsically
  (cond
    ((classp spec)
     ;; TBD - Verify this onto applications, across implementations
     (class-finalized-p spec))
    (t
     ;; Initial Prototype, NB
     (let ((impl-ctype (sb-kernel:specifier-type spec)))
       (not (typep impl-ctype 'sb-kernel:unknown-type))))))

;; (type-defined-p (make-symbol "Frob"))
;; => NIL

;; TBD ... recursive processing for TYPE-DEFINED-P
;; (type-defined-p `(or ,(make-symbol "Frob") string))
;; =(??)=> T ;; Not ideal for applications, NB
