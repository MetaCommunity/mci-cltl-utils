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


;; T.D: (MAKE-INSTANCE SINGLETON) SEMANTICS
;; => IMPLICIT SUBCLASSING (w/ uninterned class name)
;;    ... and, in a sense, lexically scoped clases.
;;
;; ... thus continuing the analogy of this API, after Garnet [KR] (KR Schemas)
;;
;; NB/TD SINGLETON => MODEL [OUTLINE]
;; - MODEL @ LTP-SYS
;;   - MODEL @ LLVM
;;   - MODEL @ ASN.1
;;   - MODEL @ UNIX (BSD)
;;   - **Subsq. ext** - Model Development @ LTP DEVO INFRAST
;;   - TBD LTP-SYS -> Data Flow
;; - MODEL @ LTP-DATA-MNG
;;   - MODEL @ SQL
;;     - NB Shared Components onto LTP-SYS -> Data Type Definition
;;   - MODEL @ IPv4 (IPv6 w/ limited adoption ??) + infrast service model
;;     - NB Shared Components onto LTP-SYS -> Data Type Definition
;;   - MODEL @ GSS-API &SUBSQ
;;     - NB Shared Components onto LTP-SYS -> Data Type Definition
;;   - **Subsq. ext** - Model Development @ LTP DEVO INFRAST - SM
;; - MODEL => FIPA-SL-MODEL (TBD)
;;     - TBD Shared Components onto LTP-SYS -> Data Flow
;; - MODEL @ XML
;;   - MODEL => XSD-MODEL
;;   - MODEL => RNC-MODEL
;;   - MODEL => DTD[MODEL
;;   - [X] XSD-MODEL -> XMI-MODEL
;;   - NB Shared Components onto LTP-SYS -> Data Type Definitions
;; - MODEL after OMG MDA/OMA/...
;;   - MODEL @ MOF
;;     - Model @ UML/MOF shared classifiers
;;     - Model @ MOF primary classifiers (cf. MOF meta-metamodel, pedagogically)
;;     - Model @ OCL
;;       - **TBD:** _Predicate-Oriented Decomposition_ of OCL Specifiers
;;       - TBD: Parsing "OCL Strings" (is not VDM, per se)
;;       - TD: _Isomorphism of_ "Parsed OCL Strings" I.R and "OCL onto XMI' I.R
;;       - NB^2 : Garnet [KR] - KR O-FORMULA Objects & KR Schema - Defn & Eval'n
;;     - Model @ UML
;;       - Model @ UML/MOF shared classifiers (see previous)
;;       - Model @ UML Metamodel
;;       - Model @ UML Profiles, Stereotypes [...]
;;         - NB Non-Isomorphism of UML Stereotypes and (ONTO) MOF Metaclasses
;;       - Model @ UML Diagram Kinds [X]
;;       - Model @ UML pedagogic visual notation
;;     - Model @ SysML
;;       - Model @ UML4SysML classifiers (& UML Profiles, Stereotypes)
;;       - Model @ SysML primary classifiers (x SysML metamodel, pedagogically)
;;       - Model @ SysML Diagram Kinds [X]
;;       - Model @ SysML (and UML) pedagogic visual notation
;;     - Model @ BPMN
;;       - Model @ BPMN primary classifiers (BPMN as a UML Profile)
;;       - Model @ "The BPMN Diagram kind" (pedagogicP
;;       - Model @ BPMN pedagogic visual notation
;;     - Model @ ODM [...] (NB: RDF impl @ librdf)
;;
;;     = Model Development @ LTP DEVO INFRAST
;;       - TBD Shared Components onto LTP-SYS -> Data Flow
;;       - TBD Shared Components @ LTP-SYS -> WHICH CLIM GARNET GTK+ OTHER?
;;       - Model Development @ LTP DEVO INFRAST / Application Environments
;;         - Model Development @ LTP DEVO INFRAST / PC Desktop Environments
;;         - Model Development @ LTP DEVO INFRAST / SRV Environments
;;         - Model Development @ LTP DEVO INFRAST / Mobile Environments
;;           - NB: Maemo (FOSS now entirely beyond Nokia) + HW & QEMU
;;           - NB: Android (FOSS w/ Google Corp't & Inc. Hegemony) + HW & QEMU
;;       - Model Development @ **LTP DEVO INFRAST - Shared Model Components**
;;         - Machine Architecture Modeling
;;           - Machine Processor Modeling (4DEVO) [xBSD, LLVM]
;;           - Machine Subarchitecture Modeling (4DEVO) [xBSD, LLVM]
;;         - OS Architecture Modeling (4DEVO)
;;           - NB: BSD UNIX systems as OS Reference Impls (UNIX)
;;         - ABI Architecture Modeling
;;           - ELF ABI Modeling
;;           - COFF ABI Modeling
;;           - Itanium ABI Modeling
;;           - TD: **Secondary ABI** Modeling @ **JNI (see also: C++)**
;;           - TD: Shared Components @ **Bytecode Component & Toolchain** Models
;;     = Model Development @ LTP DEVO INFRAST - SM
;;       - [...]

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

;; FIXME: Automatically finalize SINGLETON after initialization w/ DEFSINGLETON
;;
;; ... iff not any superclass of SINGLETON is a forward-referenced class

;; TBD: DEFMETHOD in a manner as to err (CERROR) when redefining a
;; method defined in "Some other system" - Assuming that the definition
;; source for each method may be available, at runtime, in any implementation-
;; specific manner. (One might endeavor to review the existing SLIME
;; SWANK soruce code, inasmuch, towards developing a portable
;; API singularly for static analysis of Common Lisp programs. Such an
;; API could then be exteded, towards the behavior of calling CERROR
;; when redefining a method defined in "Some other system". This also
;; assumes the definition of a concept of "System," at some granularity
;; broader than the individual source file -- vis a vis ASDF, or the
;; original CLOCC MK:DEFSYSYSTEM.
;;
;; Conesquent to such an extension, one might define an :AFTER method onto
;; FINALIZE-INHERITANCE (FORWARD-REFERENCED-CLASS) such that may be used
;; to dispatch to FINALIZE-INHERITANCE onto any ... (Refer to following src)

(eval-when ()
  (defclass frob-previous ()
    ())

  (defclass frob-fwd ()
    ()
    (:metaclass forward-referenced-class))

  (validate-class frob-fwd)

  (defclass frob-now (frob-fwd frob-previous)
    ())

  (class-direct-subclasses (find-class 'frob-fwd))

)
;; ...
;;
;; This would serve to permit a call to FINALIZE-CLASS for each
;; direct subclass of a forward referenced class -- to an effect: Ensure
;; that each SINGLETON is finalied, once every superclass of the
;; SINGLETON is finalized. This could be implemented portably, with a
;; method
;;   FINALIZE-SUBCLASS ((SUB STANDARD-CLASS) (SUPER FORWARD-REFERENCED-CLASS??))
;;
;; The convention of erring if the DEFMETHOD :AFTER form onto
;; FINALIZE-INHERITANCE would overwrite any method defined in any other
;; system would be implemented as a convenience for purposes of Common
;; Lisp systems integration -- somehwat optional to the actual
;; operations of this application protocol or reified application
;; pattern.
;;
;; Note that the latter may be defined in a manner such as to allow --
;; within a CONTINUE restart -- defining a method that would call both the
;; "Original Method" onto :AFTER and the "New Method" onto :AFTER in any
;; specified order.

;; NB Limited FINALIZE-SUBCLASS
;; - FINALIZE-SUBCLASS during FINALIZE-INHERITANCE SINGLETON
;; - Does not require any immediate DEFMETHOD-ENCAPSULATE-OTHER-METHOD behaviors

#|

 NB: Towards signaling an error (with CERROR and a CONTINUE restart defined
 specifically for this) as when a method definition overrides a method
 defined in some other system:

 - Ensure that the CONTINUE restart will dispatch to a procedure
   defining a method that will call both the "Existing Method
   Definition" and the "New Method Definition"
   - In this instance, dispatching to the "New method definition" after
     the "Existing method  definition.

 - Also define a restart (OVERRIDE) such that will override the existing
   method definition,

 - and a restart (IGNORE) such that will ignore the new method
   definition,

 - and a restart (CONTINUE-OTHER) such that will perform a similar
   procedure  as the CONTINUE restart. In this instance, the method
   resulting after the CONTINUE-OTHER restart -- instead -- would call
   the "New method definition" before the "Existing method
   definition".

  - With either the CONTINUE or CONTINUE-OTHER restart main
    function, ensure that the resulting "Dispatching method definition"
    will be defined in a "Null System". Ideally, this would serve to
    permit a distinction between the system defining  the "Existing
    method" and the system defining the "New method", within any
    subsequent systems analysis tasks.

NB: This assumes an availability of source location information,
at a granularity of system definitions in Common Lisp programs.


|#

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
