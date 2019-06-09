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

;; Ed. NB: See also Garnet [KR]

(defpackage #:ltp/common/singleton
  (:nicknames #:ltp.common.singleton)
  (:use #:ltp/common/mop
        #:ltp/common
        #:cl)
  (:export
   #:singleton
   #:defsingleton
   ))

(in-package #:ltp/common/singleton)

;; NB: File depends on:
;; #:ltp-common-mop [system]
;; finalize.lisp [source file]
;; - must be evaluated after previous DEFPACKAGE, at present


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
                                  :test #'eq)))
                    (values meta-prop params-adj)))
                 (t
                  (values (list :metaclass 'singleton)
                          params))))))
    (let ((%superclasses (append superclasses
                                 (list 'singleton))))
      ;; ^ FIXME: In effect, injecting SINGLETON into the tail of the
      ;; list of direct superclasses - here, as for evaluation in a
      ;; DEFCLASS form - this will not be sufficient for all classes
      ;; as may extend SINGLETON. In a future revision, this concern may
      ;; be addressed with a manner of initialization argument mangling
      ;; onto SHARED-INITIALIZE (SINGLETON T)
      ;;
      ;; NB: This should be considered again, in light of the
      ;; PROTOTYPE-CLASS proposal, as described -- in something of a
      ;; rough sketch reagard -- at the end of this source file.
      ;;
      ;; This macro may subsequently be updated to not use DEFCLASS at
      ;; all.
      ;;
      ;; It should provide an analogous behvior to DEFCLASS, in every
      ;; portable regard. Note e.g during SB-PCL DEFCLASS
      ;; - SB-KERNEL::%COMPILER-DEFCLASS in the compiler environment
      ;;   - Can this be approximated with a compile-time DEFTYPE ?
      ;; - SB-PCL::LOAD-DEFCLASS in normal top-level evaluation
      (multiple-value-bind (%metaclass %params)
          ;; NB: This does not check to ensure that any specified
          ;; metaclass is a subtype of SINGLETON
          ;;
          ;; NB: This metaclass support might be summarily removed, in
          ;; lieu of a SINGLETON-PROTOTYPE semntics - if not supplanted
          ;; with a :PROTOTYPE-METACLASS semantics
          (compute-metaclass params)

        ;; FIXME: Prepend the singleton prototype class to %SUPERCLASSES
        ;; ... in effect, as this may have to abandon DEFCLASS
        `(defclass ,name ,%superclasses
           ,slots
           ,%metaclass
           ,@%params)))))

;; FIXME: SINGLETON Finalization
;;
;; This sytem may automatically finalize SINGLETON after
;; initialization or -- as with change-class from a
;; FORWARD-REFERENCED-CLASS -- after reinitialization, iff not any
;; superclass of the SINGLETON is a forward-referenced class

;; ... to an effect: Ensure that each SINGLETON is finalized, once every
;; superclass of the SINGLETON is finalized.
;;


(defmethod finalize-reachable ((class singleton))
  (catch 'finalize-singleton
    (macrolet ((hcall (form)
                 `(handler-bind ((class-finalization-warning
                                  (lambda (c)
                                    (warn c)
                                    (throw 'finalize-singleton (values)))))
                    ,form)))
      (dolist (supc (class-direct-superclasses class))
        (hcall (finalize-reachable-superclass supc class)))
      (multiple-value-prog1
          (hcall (call-next-method))
        (dolist (subc (class-direct-subclasses class))
          (finalize-reachable-subclass subc class))))))


(defmethod shared-initialize :after ((instance singleton)
                                     slots &rest initargs
                                     &key &allow-other-keys)
  (declare (ignore slots initargs))
  (finalize-reachable instance))


; Tests for Singleton Finalization - e.g
#+NIL
(eval-when ()
  (macrolet ((mk (class pfx &rest dsup)
               `(make-instance
                 (quote ,class)
                 :name (quote ,(gentemp
                                (concatenate 'simple-string
                                             (symbol-name pfx)
                                             #.(symbol-name '#:_nr))))
                 ,@(when dsup
                     (list :direct-superclasses
                           (cons 'list dsup))))))

    (defparameter *s1* (mk singleton s-1))

    (values
     (class-finalized-p *s1*)
     ;; => T


     (prog2 (defparameter *s2* (mk forward-referenced-class s-2 *s1*))
         (class-finalized-p *s2*))
     ;; => NIL

     (prog2 (defparameter *s3* (mk singleton s-3 *s2* *s1*))
         (class-finalized-p *s3*))
     ;; => NIL

     ;; NB: This CHANGE-CLASS call may not be portable for applications,
     ;; per [AMOP]. In the PCL MOP implementation, it can be evaluated
     ;; when *S2* is a forward-referenced-class
     (prog2 (change-class *s2* (find-class 'singleton)
                          :name (class-name *s2*)
                          :direct-superclasses (class-direct-superclasses *s2*))
         (class-finalized-p *s2*))
     ;; => T

     (class-finalized-p *s3*)
     ;; => T

     ))
    ;; => T, NIL, NIL, T, T

  )


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
(declare (ignore class initargs))
  (find-class 'singleton-direct-slot-definition))


(defclass singleton-effective-slot-definition
    (singleton-slot-definition standard-effective-slot-definition)
  ())


(defmethod effective-slot-definition-class ((class singleton)
                                            &rest initargs)
  (declare (ignore class initargs))
  (find-class 'singleton-effective-slot-definition))



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

(typep (find-class 'singleton-1) 'singleton)
;; => T

(subtypep (find-class 'singleton-1) 'singleton)
;; => T, T

;; alternately ...
(let ((c (find-class 'singleton-1)))
  (multiple-value-bind (st-1 st-2)
      (subtypep c 'singleton)
    (multiple-value-bind (mt-1 mt-2)
        (subtypep (class-of c) 'singleton)
      (values st-1 st-2
              mt-1 mt-2))))

;; => T, T, T, T


(class-slots (find-class 'singleton-1))

(class-direct-slots (find-class 'singleton-1))

(defsingleton singleton-2 (singleton-1)
  ((sl-c)))

;; (finalize-inheritance (find-class 'singleton-2))

(class-slots (find-class 'singleton-2))

(class-direct-slots (find-class 'singleton-2))

;; -- Towards defining a subclass of SINGLETON as an instance of its own class

;; FIXME - DESIGN ISSUE

(typep (find-class 'singleton-1) 'singleton-1)
;; => NIL ; which is not good for this design


(defparameter *c1*
  (make-instance 'forward-referenced-class
                 :name (gentemp "C1-")))

(change-class *c1* (find-class 'singleton)
              :direct-superclasses (list *c1* (find-class 'singleton)))
;; ^ fails during SHARED-INITIALIZE :AFTER (SB-PCL::STD-CLASS T)
;;   ... more specifically, in SB-PCL::UPDATE-CLASS
;;       ... with a generic SIMPLE-ERROR

;; ADD-DIRECT-SUBCLASS - needed for class-system mangement, but
;; insufficent for updating the class itself

(defparameter *c2*
  (make-instance 'singleton
                 :name (gentemp "C2-")))

(add-direct-subclass (find-class 'singleton) *c2*)

(class-direct-superclasses *c2*)

;; ...

#+DNW
(setf (class-direct-superclasses *c2*)
      ;; DNW - there is no standard SETF method corresponding to this
      ;; reader function.
      ;;
      ;; PCL also does not define any setter for the metaclass slot that
      ;; CLASS-DIRECT-SUBCLASSES accesses.
      ;;
      ;; Many of those metaclass slots may be defined, in effect,
      ;; "Read-only." However, this does not in itself serve to prove as
      ;; if those slots' values cannot be usefully modified -- if
      ;; corresponding to an appropriate modification of class layout
      ;; information, in an implementation-specific regard.
      (cons *c2* (class-direct-superclasses *c2*)))

;; No way to work around that pedagogic quirk in UPDATE-CLASS ?
;;
;; Even if the DIRECT-SUPERCLASSES list is destructively modified
;; for the class, PCL might still cause it to fail during SHARED-INITIALIZE



(defparameter *c3*
  (make-instance 'singleton
                 :direct-superclasses  (list (find-class 'singleton))
                 :name (gentemp "C3-")))

;; (typep *c3* 'singleton)

;; (subtypep *c3* 'singleton)
;;  => T, T

;; -- [!!] --

;; There may be a way to work around that pedagogic quirk in PCL,
;; by using an anonymouns intermediary class representative of
;; the user-specified superclass information - such that the
;; user-named class will be a subclass and an instance of that
;; class (but not a subclss of itself)
;;
;; Such as class may be denoted as the PROTOTYPE-CLASS of a SINGLETON
;;
;; This bit of indirection will require an update onto DEFSINGLETON


)


(defun* find-superclass (super class &optional (errorp t))
  ;; return the first class found, of which SUPER it is a direct superclass
  ;; or the class itself if SUPER is equivalent to CLASS
  (declare (type class-designator super class)
           (values &optional class))
  (labels ((handle-fail ()
             (cond
               (errorp
                (error "No superclss ~S found for ~S"
                       super class))
               (t (values)))))
    (let ((%super (compute-class super errorp))
          (%class (compute-class class errorp)))
      (cond
        ((eq %class %super) (values %class))
        ((eq %class (load-time-value (find-class 't)
                                     t))
         (handle-fail))
        (t
         (dolist (c (class-direct-superclasses %class)
                  (handle-fail))
           (cond
             ((eq c %super) (return %class))
             (t (let ((found
                       (find-superclass %super c nil)))
                  (when found
                    (return c)))))))))))
#+NIL
(eval-when () ;; instance tests - FIND-SUPERCLASS

  (defclass f-1 ()
    ())

  (defclass f-2 (f-1)
    ())

  (defclass f-3 (f-2)
    ())

  (find-superclass 'f-1 'f-3)
  => #.(find-class 'f-2)

  (find-superclass 'g-1 'f-3 nil)
  => no value returned

  (defclass g-2 () ())

  (find-superclass 'g-2 'f-3 nil)
  => no value returned
)

;; FIXME: use (FIND-SUPERCLASS 'singleton ...) in DEFSINGLETON*


(defmacro defsingleton* (name (&rest superclasses)
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
                                  :test #'eq)))
                    (values (cadr meta-prop) params-adj)))
                 (t
                  (values 'singleton params))))))
    (let ((%proto-name
           ;; FIXME: Allow this to be specified in PARAMS (??)
           (make-symbol (concatenate 'simple-string
                                    (symbol-name name)
                                    (symbol-name '#:-prototype)))))
      (with-symbols (%proto %superclasses %supers
                            %fwd %c %cdef %fwdc
                            %singleton )
        (multiple-value-bind (%metaclass %params)
            (compute-metaclass params) ;; NB: May be unused here

          ;; NB: This mayx fail if any of the superclasses is a
          ;; forward-referenced class - that can be worked around, locally.

          ;; NB: Using an implcit PROTOTYPE-METACLASS := SINGLETON here.
          ;;     This may be updated, in a later revision.
          `(let* ((,%fwd nil)
                  (,%superclasses (quote ,superclasses))
                  (,%supers
                   (mapcar #'(lambda (,%c)
                               (let ((,%cdef (find-class ,%c nil)))
                                 (cond
                                   (,%cdef (values ,%cdef))
                                   (t
                                    ;; FIXME: Consider using
                                    ;; ENSURE-CLASS here
                                    (let ((,%fwdc (make-instance
                                                   'forward-referenced-class
                                                   :name ,%c)))
                                      (setf ,%fwd
                                            (nconc ,%fwd (list ,%fwdc)))
                                      (values ,%fwdc))))))
                           ,%superclasses)))

             ;; T.D: Emit a style-warning about each class in %FWD
             ;;
             ;; TBD: Also define a rudimentary DEFTYPE ... CLASS
             ;; within the compiler environment, for each class in %FWD

             (let ((,%singleton (load-time-value (find-class 'singleton)
                                                 t)))
               (unless (some (lambda (c)
                               (find-superclass ,%singleton c nil))
                             ,%supers)
                 (setf ,%supers (nconc ,%supers (list ,%singleton)))))

             (let ((,%proto (ensure-class (quote ,%proto-name)
                                          :metaclass (quote ,%metaclass)
                                          :direct-superclasses ,%supers)))
               ;; NB: ^ This may not be sufficent as to be able to
               ;; locate the class during evaluation of the following
               (values
                (defclass ,name ,(cons %proto-name superclasses)
                  ,slots
                  (:metaclass ,%proto-name)
                  ,@%params)
                ,%proto
                ,%fwd))))))))

(eval-when ()

  (defclass frob-c ()
    ())


  (defsingleton* frob-s-1 (frob-c)
    ())

  (typep (find-class 'frob-s-1) 'frob-c)
  ;; => T

  (subtypep (find-class 'frob-s-1) 'frob-c)
  ;; => T, T

  (typep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; => T

  ;; So, THIS WORKS

  ;; though it's indecorus as for the default prototype class naming a.t.m

  (subtypep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; FIXME => NIL, T
  ;; ^ dispatch in INITIALIE-INSTANCE such as to create an instance of
  ;; the prototype clss of FROB-S-1 w/ FROB-S-1 as a superclass

)
