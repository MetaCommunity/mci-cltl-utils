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

;; (least-common-superclass 'standard-object 'structure-object)


;; ---- Portable INSTANCE (not CONDITION) alias type

(deftype instance ()
  (load-time-value (class-name
                    #+NIL
                    (least-common-superclass
                     (find-class 'condition)
                     (least-common-superclass 'standard-object
                                              'structure-object))
                    #-NIL
                    (least-common-superclass 'standard-object
                                             'structure-object))
                   t))

;; (typep (class-prototype (find-class 'standard-class )) 'instance)
;; => T

;; (typep (class-prototype (find-class 'structure-class )) 'instance)
;; => T

;; QA - INSTANCE deftype & [SBCL]
;;
;; (typep (class-prototype (find-class 'condition)) 'sb-pcl::slot-object)
;; => NIL
;;
;; Subsq ...
;;
;; (typep (class-prototype (find-class 'condition)) 'instance)
;; => NIL
;;
;; NB [SBCL]
;;    (class-precedence-list (find-class 'condition))
;; ... which may seem inconsistent onto the above TYPEP calls.
;;
;; Subsq ...
;;
;; [Indemnification Clause Here]

#+NIL
(eval-when ()
  ;; QA - test the INSTANCE type for consistency onto an initialized
  ;; CONDITION object
  ;;
  ;; - tested with SBCL 1.4.16.debian
  ;;   - TYPEP CONDITION 'INSTANCE => NIL
  (handler-bind ((condition (lambda (c)
                              (warn "FROB: ~S ~S"
                                    (typep c 'instance)
                                    (class-of c)))))
    (signal 'condition))

)
