;; singleton.lisp - ltp-common-mop-singleton protocol definitions
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------


(in-package #:cl-user)

(defpackage #:ltp/common/mop/singleton
  (:nicknames #:ltp.common.mop.singleton)
  (:use #:ltp/common/mop
        #:ltp/common
        #:cl)
  (:export
   #:singleton
   #:defsingleton
   ))

(in-package #:ltp/common/mop/singleton)


;; -- Singleton Class Definition

(defclass singleton (standard-class)
  ())


(validate-class singleton)


(defmacro defsingleton (name (&rest superclasses)
                                slots
                        &rest params)
  (labels ((compute-metaclass (params)
             (let ((meta-n (position :metaclass
                                     (the list params)
                                     :test #'eq
                                     :key #'car)))
               (cond
                 (meta-n
                  (let* ((meta-prop
                          (nth meta-n params))
                         (params-adj
                          (remove meta-prop params
                                  :test #'wq)))
                    (values meta-prop params-adj)))
                 (t
                  (values (list :metaclass 'singleton)
                          params))))))
    (let ((%superclasses (append superclasses
                                 (list 'singleton))))
      (multiple-value-bind (%metaclass %params)
          (compute-metaclass params)
        `(defclass ,name ,%superclasses
           ,slots
           ,%metaclass
           ,@%params)))))

;; FIXME: Automatically finalize SINGLETON after initialization.
;;
;; (NB: SINGLETON is not a forward-referenced class)


;; -- Singleton Slot Definitions

(defclass singleton-slot-definition (standard-slot-definition)
  ()
  (:default-initargs
   :allocation :class))


(defclass singleton-direct-slot-definition
    (singleton-slot-definition standard-direct-slot-definition)
  ;; NB: These will extend the standard MOP classes
  ;;
  ;; The allocation meta-slot will be by-in-large unused here - assumed
  ;; to be, in effect, always :CLASS
  ())

;; FIXME: Err for any singleton slot definition with
;; :ALLOCATION :INSTANCE specified during slot metaobject
;; initialization


(defmethod direct-slot-definition-class ((class singleton)
                                         &rest initargs)
  (find-class 'singleton-direct-slot-definition))


(defclass singleton-effective-slot-definition
    (singleton-slot-definition standard-effective-slot-definition)
  ())


(defmethod effective-slot-definition-class ((class singleton)
                                            &rest initargs)
  (find-class 'singleton-effective-slot-definition))


;; MOP Interop ...

;; --

;; Applications TD
;; - Use for internal representation of "Application Personalities" in
;;   the Lisp Environment - Class definitions, method definitions, and
;;   runtime application protocols.
;;
;; - Use for internal representation of bytecode ABIs (host ABIs,
;;   and toolchain ABIs)
;;
;; - Extend SINGLETON => SINGLETON-FUNCALLABLE-INSTANCE
;;
;;   - Use in definitions of external API forms - onto external type
;;     schema, as also extending SINGLETON)
;;
;;   - Observe XMI as a serialized representation of MOF (subsq. UML and
;;     other metamodels tractably extending OMG MOF) towards
;;     consideration with regards to portable definitions for such type
;;     schema. (NB: MOF and RDF may occupy different domains, per se.
;;     Note, however, the perhaps visually-oriented syntax of the RDF
;;     metamodel defined in OMG ODM)
;;
;;   - Note that this is not, per se, for redefining any single manner
;;     of knowledge representation system

;; TBD: SINGLETON-FUNCALLABLE-INSTANCE & CLASS-OWNED-METHOD definitions
;;
;; - NB: Common Lisp GENERIC-FUNCTION metaobjects as globally scoped
;;       objects, principally extending on a semantics of the Lisp
;;       function as a first order object, for purposes of definining
;;       polymorphic functions in Common Lisp programs
;;
;; - NB: Polymorphic functions in Simula and subsq.
;;
;; - NB: Type signatures in SML


;; TBD: SINGLETON usage in _static dispatching_ w/ portable extensions
;; onto CLtL2+MOP standard generic functions and standard methods - at
;; least insofar as may be portably integrated with any number of PCL
;; implementations.

#+nil
(eval-when ()

(defsingleton singleton-1 ()
  ((sl-1)
   (sl-b)))

(let ((c (find-class 'singleton-1)))
  (finalize-inheritance c)
  (multiple-value-bind (st-1 st-2)
      (subtypep c 'singleton)
    (multiple-value-bind (mt-1 mt-2)
        (subtypep (class-of c) 'singleton)
      (values st-1 st-2
              mt-1 mt-2))))

;; => T, T, T, T


(finalize-inheritance (find-class 'singleton-1))


(class-slots (find-class 'singleton-1))

(class-direct-slots (find-class 'singleton-1))



(defsingleton singleton-2 (singleton-1)
  ((sl-c)))

(finalize-inheritance (find-class 'singleton-2))

(class-slots (find-class 'singleton-2))

(class-direct-slots (find-class 'singleton-2))


)
