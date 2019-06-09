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


;; NB
(typep (find-class 'singleton-2) 'singleton-1)
;; => NIL
;; ^ there it differs from the newer DEFSINGLETON*

;; -- Towards defining a subclass of SINGLETON as an instance of its own class

;; FIXME - DESIGN ISSUE

(typep (find-class 'singleton-1) 'singleton-1)
;; => NIL ; which is not good for this design
;;
;; NB: The singleton-subtype concern has been addressed - insofar as with
;; regards to direct superclasses - in the present DEFSINGLETON*
;; definition

(typep (make-instance 'singleton-1) 'singleton-1)
;; => T ; which is passable

--

(defsingleton singleton-3-1 (singleton-1)
  ())

(typep (make-instance 'singleton-3-1) 'singleton-1)
;; => T

(typep (make-instance 'singleton-3-1) 'singleton-3-1)
;; => T

(typep (find-class 'singleton-3-1) 'singleton-1)
;; => NIL ;; not good though

--

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
;; ^ ADD-DIRECT-SUBCLASS may not affect the SUBCLASS itself

;; ...

#+TBD
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


(defun* find-class-in-precedence (super class &optional (errorp t))
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
                       (find-class-in-precedence %super c nil)))
                  (when found
                    (return c)))))))))))
#+NIL
(eval-when () ;; instance tests - FIND-CLASS-IN-PRECEDENCE

  (defclass f-1 ()
    ())

  (defclass f-2 (f-1)
    ())

  (defclass f-3 (f-2)
    ())

  (find-class-in-precedence 'f-1 'f-1)
  => #.(find-class 'f-1)

  (find-class-in-precedence 'f-1 'f-3)
  => #.(find-class 'f-2)

  (find-class-in-precedence 'g-1 'f-3 nil)
  => no value returned

  (defclass g-2 () ())

  (find-class-in-precedence 'g-2 'f-3 nil)
  => no value returned
)

;; FIXME: use (FIND-CLASS-IN-PRECEDENCE 'singleton ...) in DEFSINGLETON*


;; NB
;; (typep (find-class 'standard-class) 'standard-class)
;; => T

(defclass singleton* (singleton)
  ((%prototype-metaclass
    ;; should be EQ to the class itself
    ;; ... at least in any subclass
    :initarg :%prototype-metaclass
    :type class
    ))
  (:metaclass singleton))


(defmacro getf* (place property &optional default)
  ;; FIXME - move definition to ltp-common
  (with-symbols (dflt obj)
    `(let* ((,dflt (load-time-value (make-symbol
                                     #.(symbol-name (quote :%default)))
                                    t))
            (,obj (getf ,place ,property ,dflt)))
       (declare (dynamic-extent ,dflt))
       (cond
         ((eq ,obj ,dflt)
          (values ,default nil))
         (t
          (values ,obj t))))))


#+NIL
(defmethod allocate-instance ((class singleton*)
                              &rest initargs)
  ;; Prototype (Unused)
  ;;
  ;; NB: MAKE-INSTANCE <CLASS> considered contradictory, if it cannot be
  ;; portably accompanied with any form to ensure that the compiler will
  ;; recognize the instance as both a type and a class.

  (let (initargs-updated)
    (macrolet ((dispatch
                   `(cond
                      (initargs-updated
                       (apply #'call-next-method class initargs))
                      (t (call-next-method)))))
      (multiple-value-bind (metaclass mc-p)
          (getf* initargs :metaclass)
        (declare (ignore metaclass))
        (unless mc-p
          (prog1 (setf (getf initargs :metaclass)
                       (make-instance ... ;; NOPE
                                      ))
            (setq initargs-updated t))))
    (dispatch))))



#+NIL
(defmethod shared-initialize ((instance singleton*) slots
                              &rest initargs
                                &key (prototype-class nil pm-p)
                                  &allow-other-keys)
  ;; NB: The PROTOTYPE-CLASS should be the class of the INSTANCE itself
  )




(defmacro defsingleton* (name (&rest superclasses)
                                slots
                         &rest params)
  ;; FIXME - due to the convention of always creating a new, compiler-
  ;;         unregistered prototype-class, as approched in this updated
  ;;         defsingleton macro -- in its present revision -- this macro
  ;;         cannot be evaluated twice for the same class.

  ;; NB: As one purpose that this new DEFSINGLETON* serves, juxtaposed
  ;;     to the original DEFSINGLETON definition: This macro definition
  ;;     serves to ensure -- at least insofar as to a perspective of
  ;;     instance/superclass and metaclass relations in CLOS -- that the
  ;;     resulting SINGLETON instance will be (FIXME: Shouild be) at
  ;;     least a subtype (in such a regard) to every class specified in
  ;;     SUPERCLASSES.
  ;;
  ;;     ----
  ;;     FIXME: As this presently uses ENSURE-CLASS to create a
  ;;     metaclass -- in a phrase, a PROTYPE-CLASS for the resulting
  ;;     SINGLETON -- such that, in that effect, may not be registered
  ;;     to the implementation type system in such a way as might be
  ;;     approached via DEFCLASS ... this system may presently lend
  ;;     to some spurious results, in testing forms.
  ;;     ----
  ;;     Subsequently, this should be updated to use DEFCLASS to define
  ;;     the PROTOTYPE-CLASS as a class named either according to a
  ;;     default naming convention local to this macro definigion, or
  ;;     defined with a user-specified :PROTYPE-METACLASS name. This
  ;;     macro definition may also be updated to include any
  ;;     user-specified :METACLASS in the class precedence list of the
  ;;     resulting PROTOTYPE-CLASS -- to an effect, the resulting CLASS
  ;;     will at least be TYPEP onto the user-specified METACLASS, even
  ;;     if the resulting CLASS will not be an instance of that
  ;;     METACLASS itself.
  ;;     ----
  ;;     NB: This documentation, in effect, uses the term PROTOTYPE-CLASS in
  ;;     lieu of METACLASS as it denotes -- in the former -- a class
  ;;     defined during evaluation of DEFSINGLETON*, however serving
  ;;     in a role of a conventional CLOS metaclass.
  ;;     ---
  ;;
  ;;     Considering limitations extended of MOP implementations, it
  ;;     may be impossible to produce a portable definition of a class
  ;;     such that will be TYPEP of its own class, anywhere beyond
  ;;     STANDARD-CLASS.
  ;;
  ;;     After refactoring some of a subtype semantics onto a class'
  ;;     metaclass -- i.e onto the class M of which the class C is
  ;;     itself an instance -- it may be assumed that some additional
  ;;     concerns can be addreessed with regards to typing for SINGLETON
  ;;     instances. In that regard, this macro provides something of an
  ;;     update to the original DEFSINGLETON macro definition.
  ;;
  ;;     This updated DEFSINGLETON* may serve to address some concerns
  ;;     with regards to SINGLETON instance typing, even if this cannot
  ;;     portably emulate the "Metacircular" character of STANDARD-CLASS
  ;;     -- there being, simply, no portable API provided for extending
  ;;     of however that was addressed for STANDARD-CLASS, in each MOP
  ;;     implementation, itself.

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
           ;; FIXME: Also provide the class, itself, as the value of
           ;;        a :singleton-prototype initarg or
           ;;        SINGLETON-PROTOTYPE access for the resulting
           ;;        class named NAME - such that would serve to
           ;;        differentiate the automatically created prototype
           ;;        class from the list of user-specified direct
           ;;        superclasses
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
                               (find-class-in-precedence ,%singleton c nil))
                             ,%supers)
                 (setf ,%supers (nconc ,%supers (list ,%singleton)))))

             (let ((,%proto (ensure-class (quote ,%proto-name)
                                          :metaclass (quote ,%metaclass)
                                          :direct-superclasses ,%supers)))
               ;; NB: ^ This may not be sufficent as to be able to
               ;; locate the class during evaluation of the following
               ;;
               ;; NB ^ As it creates an instance of a class metaobject
               ;; via MAKE-INSTANCE (and not DEFCLASS) this _may not_
               ;; serve to register the prototype-class (resulting from
               ;; that evaluation) as a class completely "known" to the
               ;; implementation type system
               ;;
               ;; So, FIXME: Use interned class names for prototype
               ;; classes created via DEFSINGLETON*; use DEFCLASS to
               ;; define such prototype classes
               ;;
               ;; Subsq FIXME: Determine some way to include any
               ;; user-specified :METACLASS as a superclass of the
               ;; resulting defsingleton* prototype-class - along with
               ;; any user-specified superclasses for the resulting
               ;; SINGLETON metaobject
               (values
                (defclass ,name ,(cons %proto-name superclasses)
                  ,slots
                  (:metaclass ,%proto-name)
                  ,@%params)
                ;; NB: The %PROTO class gets too easily lost here,
                ;; beyond prototyping
                ,%proto
                ;; FIXME: Emit a signal or a style-warning for each
                ;; foward referened class. Note that these prevent the
                ;; resulting singleton class from being, as yet,
                ;; finalized.
                ,%fwd))))))))

;; FIXME: Test DEFSINGLETON* with forward-referenced direct superclasses

;; FIXME: Test DEFSINGLETON* with a SINGLETON superclass of a direct superclass

(eval-when ()

  (defclass frob-c ()
    ())


  (defsingleton* frob-s-1 (frob-c)
    ;; FIXME May err if evaluated twice - see above remarks
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

#+TBD
(eval-when ()

(defsingleton* singleton-1-1 ()
  ((sl-1)
   (sl-b)))

(typep (find-class 'singleton-1-1) 'singleton)
;; => T

(subtypep (find-class 'singleton-1-1) 'singleton)
;; => T, T

;; alternately ...
(let ((c (find-class 'singleton-1-1)))
  (multiple-value-bind (st-1 st-2)
      (subtypep c 'singleton)
    (multiple-value-bind (mt-1 mt-2)
        (subtypep (class-of c) 'singleton)
      (values st-1 st-2
              mt-1 mt-2))))

;; => T, T, T, T


(class-slots (find-class 'singleton-1-1))q

(class-direct-slots (find-class 'singleton-1-1))


(defsingleton* singleton-2-2 (singleton-1-1)
  ((sl-c)))


(class-slots (find-class 'singleton-1-2))

(class-direct-slots (find-class 'singleton-1-2))

(typep (find-class 'singleton-1-1) 'singleton-1-1)
;; => NIL ; which still is not good for this design

(typep (find-class 'singleton-2-2) 'singleton-1-1)
;; => T ;; NB "OK" (this differs from the original DEFSINGLETON)
;;
(subtypep (class-of (find-class 'singleton-2-2)) 'singleton-1-1)
;; => T, T



(typep (make-instance 'singleton-1-1) 'singleton-1-1)
;; => T ;; which is passable,
;;         but really no different than  STANDARD-OBJECT semiotics


(subtypep (make-instance 'singleton-1-1) 'singleton-1-1)
;; => NIL, T ;; not OK

(subtypep (make-instance 'singleton-2-2) 'singleton-1-1)
;; => NIL, T ;; not OK


;; to make a suclass C2 of a class C1 both an instance and subtype of C1 ...
;;
;; define MAKE-INSTANCE => CREATE-SUBTYPE

;; NB: However approached - whether by prepending a singleton S to the
;; list of direct superclasses specified originally to MAKE-INSTANCE, or
;; if otherwise - this change will also affect the outcomes of the
;; original DEFSINGLETON (no prototype-class) macro definition.



)

;; #+TESTING
(eval-when ()
(defmethod shared-initialize ((instance singleton) slots
                              &rest initargs &key
                                               direct-superclasses
                                               &allow-other-keys)
  ;; ensure that any call to MAKE-INSTANCE of a SINGLETON will produce
  ;; an object that is a subtype of the original SINGLETON [FIXME DNW]
  (let (initargs-updated)
    (macrolet ((dispatch ()
                 `(cond
                    (initargs-updated
                     (apply #'call-next-method instance slots initargs))
                    (t (call-next-metod)))))
      (labels ((ensure-superclass (c)
                 (cond
                   (direct-superclasses
                    (unless (some (lambda (c)
                                    (find-class-in-precedence instance c nil))
                                  direct-superclasses)
                      (prog1 (setf (getf initargs :direct-superclasses)
                                   (cons instance direct-superclasses))
                        (setq initargs-updated t))))
                   (t (prog1 (setf (getf initargs :direct-superclasses)
                                   (cons instance direct-superclasses))
                        (setq initargs-updated t))))))
        (ensure-superclass instance)
        (dispatch)))))

)

(eval-when ()
  (defsingleton* x-a ()
    ())

  (typep (make-instance 'x-a) 'x-a)
  ;; => T ;; consistent onto CLOS

  (subtypep (make-instance 'x-a) 'x-a)
  ;; FIXME while => NIL, T

  ;; NB/ORTHOGONAL - CLOS and MOP
  (values (find-class nil nil)
          (let* ((nm (gentemp "singleton-"))
                 (c (make-instance 'x-a :name nm)))
            (find-class nm nil)))
  ;; => NIL, NIL
  ;;
  ;; THUS: While MAKE-INSTANCE of a STANDARD-CLASS does result in an
  ;; object that is (TYPEP object 'STANDARD-CLASS) but it may not
  ;; result in the compiler type system recognizing the instance as
  ;; representing a defined class. In that regard, it would seem as
  ;; though the class-name has no consistent effect, until the class is
  ;; registered -- via non-portable code -- in the implementation type
  ;; system.
  ;;
  ;; FIXME - VULN ANALYSIS: Theoretically, it may be possible to use
  ;; MAKE-INSTANCE to create a STANDARD-CLASS that has a name EQ to any
  ;; defined system class, while that named but "Unknown" class would
  ;; not be EQ to the corresponding system class, in the
  ;; implementation.


;; ...

  (defsingleton* x-b (x-a)
    ;; FIXME: New DEFSINGLETON* DNW for any forward-referenced superclass
    ())

  (typep (make-instance 'x-b) 'x-b)
  ;; => T ;; consistent onto CLOS

  (typep (make-instance 'x-b) 'x-a)
  ;; => T ;; consistent onto CLOS

  (subtypep (make-instance 'x-b) 'x-a)
  ;; FIXME while => NIL, T

  (subtypep (find-class 'x-b) 'x-a)
  ;; => T, T
  ;; should be same, with or without the added SHARED-INITIALIZE
  ;; as it's consistent onto CLOS


;; -- test the new SHARED-INITIALIZE method with the original DEFSINGLETON macro
  (defsingleton x-c ()
    ())
  (typep (make-instance 'x-c) 'x-c)
  ;; => T ;; consistent onto CLOS


  (class-direct-superclasses (find-class 'x-c))

  ;; -

  (defsingleton x-d (x-c)
    ())
  (typep (make-instance 'x-d) 'x-d)
  ;; => T ;; consistent onto CLOS

  (subtypep (make-instance 'x-d) 'x-c)
  ;; FIXME while => NIL, T
  ;; same behavior as with new DEFSINGLETON* [FIXME there too]


  (class-direct-superclasses (find-class 'x-d))

  (subtypep 'x-d 'x-c)
  ;; => T, T ;; consistent onto CLOS


)


;; --


;; T.D: Also test this type/instance/subtype semantics for CHANGE-CLASS
;; from a FORWARD-REFERENCED-CLASS to SINGLETON
