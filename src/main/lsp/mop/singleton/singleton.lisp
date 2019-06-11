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



;; --

(macrolet ((find-for (name accessor whence base-class)
             `(block search
                (dolist (c (reverse (class-precedence-list
                                     (find-class (quote ,base-class)))))
                  (dolist (sl (,whence c))
                    (when (find (quote ,name)
                                (the list (,accessor sl))
                                :test #'eq)
                      (return-from search (slot-definition-name sl))))))))
  (defconstant* +direct-superclasses-slot+
      (or (find-for :direct-superclasses
                    slot-definition-initargs
                    class-slots
                    standard-class)
          (find-for class-direct-superclasses
                    slot-definition-readers
                    class-direct-slots
                    standard-class)
          (error "Cannot find direct superclasses slot for ~
standard-class, in this implementation"))))

;; --

(defmethod shared-initialize :after ((instance singleton) slots
                                     &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (or (eq slots t)
            (and (consp slots)
                 (find +direct-superclasses-slot+ (the cons slots)
                       :test #'eq)))
    (let ((tgt-class (find-class 'singleton))
          (unused-class (find-class 'standard-object))
          (dsup (slot-value instance +direct-superclasses-slot+)))
      (declare (dynamic-extent tgt-class unused-class))

      (setq dsup
            (delete unused-class dsup :test #'eq))

      (unless (some (lambda (c)
                      (find-class-in-precedence tgt-class c nil))
                    dsup)
        (setq dsup (cons tgt-class dsup)))

      #+NIL
      (warn "Frob in SHARED-INITIALIZE for ~S - DSUP ~S" instance dsup)

      (setf (slot-value instance +direct-superclasses-slot+)
            dsup)

      ;; NB
      (finalize-reachable instance))))


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


  (defsingleton s-x-1 ()
    ())

  (defsingleton s-x-2 (s-x-1)
    ())

  (typep (find-class 's-x-2) 's-x-1)
  ;; => NIL ; which is - in short - why this was updated with DEFSINGLETON
  ;;
  ;; NB: A broader synposis about the DEFSINGLETON update is provided,
  ;; in commentary available in the macro definition's source form, below.

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


;; NB: DEFSINGLETON Update
;;
;; The origingial DEFSINGLETON macro definition was removed in changeset
;; d2d9c76740f684a18276207cc149a981e76619a3 - 9 June, 2019 19:01:08-0700
;;
;; Subsequently, the updated DEFSINGLETON* has been renamed DEFSINGLETON,
;; in effect replacing the original macro definition.

;; NB: Refer to DEFSINGLETON test definitions at end of file


;; -- misc utils (prototypes)

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



#+NIL ;; NB: Used in some ealier prototypes, retained here temporarily
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



(defgeneric prototype-implementation-class (class))
(declaim (ftype (function (class)
                          (values class-designator &optional))
                prototype-implementation-class))

(defgeneric (setf %prototype-implementation-class) (new-value class))
(declaim (ftype (function (class-designator class)
                          (values class-designator &optional))
                (setf %prototype-implementation-class)))


;; NB: Considering that the class PROTOTYPE-CLASS is used in the
;; evaluation of the updated DEFSINGLETON maro, it cannot be defined with
;; DEFSINGLETON*.
;;
;; The class represents a SINGLETON type - equivalently, representing a
;; SINGLETON metaclass. As it defines the class PROTOTYPE-CLASS,
;; however, it  will not be defined with a PROTOTYPE metaclass of type
;; PROTOTYPE-CLASS, such as how any class defined with DEFSINGLETON will be
;; defined.
;;
;; As such, the class of PROTOTYPE-CLASS will not be capable of storing
;; a refeernce to the class, PROTOTYPE-CLASS, itself. Although this
;; could be addressed with some indirection, it might not be asumed to
;; represent any serious concern for applications.
;;
(defclass prototype-class (singleton)
  ((implementation-class
    :type class-designator
    :initarg :implementation-class
    :reader prototype-implementation-class
    :writer (setf %prototype-implementation-class)))
  (:metaclass singleton))


;; --

(defmacro defsingleton (name (&rest superclasses)
                                slots
                        &rest params)

  ;; NB: As one purpose that this new DEFSINGLETON serves, juxtaposed
  ;;     to the original DEFSINGLETON definition: The resulting
  ;;     SINGLETON instance, as defined with this updated macro, will
  ;;     represent a subtype to every class specified in SUPERCLASSES.
  ;;
  ;;     --
  ;;     NB: This documentation, in effect, uses the term PROTOTYPE-CLASS in
  ;;     lieu of METACLASS as it denotes -- in the former -- a class
  ;;     defined during evaluation of DEFSINGLETON, however serving
  ;;     in a role of a conventional CLOS metaclass.
  ;;     ---
  ;;
  ;;     Considering limitations as extended of MOP implementations, it
  ;;     may be impossible to produce a portable definition of a class
  ;;     such that will be TYPEP of its own class -- anywhere beyond
  ;;     STANDARD-CLASS.
  ;;
  ;;     For any singleton class C2 that may be defined as a subclass of
  ;;     a class C1 -- such as for any purpose of extending the
  ;;     semantics of the class C1 onto the definition of C2 -- after
  ;;     addressing some of the superclass semantics onto the metaclass
  ;;     of C2 -- i.e onto the class M of which the class C is itself an
  ;;     instance -- such that M will be a subtype of C2, as well as C1
  ;;     being a subtype of C2 -- it may thus be ensured that the singleton
  ;;     instance C2 is, at least, a subtype of those types specified in
  ;;     the instance's direct superclasses list. This may serve to
  ;;     alleviate some concerns with regards to typing for SINGLETON
  ;;     instances, in applications.
  ;;
  ;;     In that regard, this macro definition provides something of an
  ;;     update to the original DEFSINGLETON macro definition.
  ;;
  ;;
  ;;     This, of course, is still not sufficient to portably emulate
  ;;     the Metacircular character of STANDARD-CLASS -- there being,
  ;;     simply, no portable API available for extending of any
  ;;     meachanisms by which STANDARD-CLASS was defined in a manner as
  ;;     to represent an instance of itself, in any implementation.

  (labels ((compute-user-metaclass (params)
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
                    (values (cdr meta-prop) params-adj)))
                 (t
                  (values nil params)))))

           (compute-prototype-class-name (params)
             (let ((name-n (position :prototype-class
                                     (the list params)
                                     :test #'eq
                                     :key #'car)))
               (cond
                 (name-n
                  (let* ((name-prop
                          (nth name-n params))
                         (params-adj
                          (remove name-prop params
                                  :test #'eq)))
                    (values (cadr name-prop) params-adj)))
                 (t
                  ;; NB: Calling INTERN before the macroexpansion @ default name
                  (values (intern (concatenate 'simple-string
                                               (symbol-name name)
                                               #.(symbol-name '#:-prototype)))
                          params))))))

    (multiple-value-bind (%proto-name %params)
        (compute-prototype-class-name params)

      (with-symbols ()

        (multiple-value-bind (user-metaclass %params)
            (compute-user-metaclass %params)

          ;; NB: Using an implcit PROTOTYPE-METACLASS is-a SINGLETON here.

          `(progn

             ;; NB: This implementation, in effect, prevents any usage
             ;; of forward-referenced classes in the SUPERCLASSES list

             (defclass ,%proto-name (,@user-metaclass prototype-class
                                                      ,@superclasses)
               ()
               (:implementation-class . ,name)
               (:metaclass prototype-class))

             (prog1 (defclass ,name (,@superclasses #+NIL singleton)
                      ;; NB: see INITILAIZE-INSTANCE below
                      ,slots
                      (:metaclass ,%proto-name)
                      ,@%params)
               ;; NB/DOCUMENTATION (and PCL) - NOTE THE FOLLOWING
               (shared-initialize (find-class (quote ,name))
                                  (list +direct-superclasses-slot+))
               (setf (%prototype-implementation-class
                      (find-class (quote ,%proto-name)))
                     (find-class (quote ,name)))

               )))))))


;; --

;; FIXME: Test DEFSINGLETON with a SINGLETON superclass of a direct
;; superclass not repreenting a SINGLETON

(eval-when ()

  (defclass frob-c ()
    ())


  (defsingleton frob-s-1 (frob-c)
    ())

  (class-direct-superclasses (find-class 'frob-s-1))
  ;; =>( #.(... SINGLETON) #.(... FROB-C))

  (typep (find-class 'frob-s-1) 'frob-c)
  ;; => T

  (subtypep (find-class 'frob-s-1) 'frob-c)
  ;; => T, T

  (typep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; => T

  ;; So, THIS WORKS

  (subtypep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; FIXME => NIL, T
  ;; ^ T.D: Dispatch in INITIALIZE-INSTANCE SINGLETON ??
  ;;   such as to create an instance of the prototype class of FROB-S-1
  ;; w/ FROB-S-1 as a direct superclass (whether or not the resulting
  ;; SINGLETON may be processed by the compiler, in any way equivalent
  ;; to an explicit DEFCLASS form, or entirely recognized by the type
  ;; system as - in fact - a class)

  #+NIL ;; useless, it would seem
  (shared-initialize (find-class 'frob-s-1) t
                     :direct-superclasses (list (find-class 'frob-c)))

  (class-direct-superclasses (find-class 'frob-s-1))

  (class-precedence-list (find-class 'frob-s-1))

  (class-precedence-list (class-of (find-class 'frob-s-1)))

  (defsingleton frob-s-1-1 (frob-s-1)
    ())

  (typep (find-class 'frob-s-1-1) 'frob-s-1)
  ;; => T ! for the singleton superclass
  ;; - suitable for some OO protocol definitions
  ;; - nearly works around limitations imposed by the implementation,
  ;;   in effect disallowing any class except STANDARD-CLASS to be an
  ;;   instance of itself.
)

#+TBD
(eval-when ()

(defsingleton singleton-1-1 ()
  ((sl-1)
   (sl-b)))

(class-direct-superclasses (find-class 'singleton-1-1))
;; NB: ^ should always include SINGLETON

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


(class-slots (find-class 'singleton-1-1))

(class-direct-slots (find-class 'singleton-1-1))


(defsingleton singleton-2-2 (singleton-1-1)
  ((sl-c)))


(class-slots (find-class 'singleton-2-2))

(class-direct-slots (find-class 'singleton-2-2))

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

(typep (make-instance 'singleton-2-2) 'singleton-1-1)
;; => T
;; "It's a start!" ....


;; NB: To produce a suclass C2 of a class C1, such that C2 is both an
;; instance typep C1 and a subtype of C1 ...
;;
;; one may define MAKE-INSTANCE => CREATE-SUBTYPE
;; via ALLOCATE-INSTANCE
;;
;; This, albeit, may seem to pose some difficulty for typing in the
;; implementation compiler (??() whether or not that may be singularly
;; dependent on any one manner of approach.

)


;; --------------------------------------------------


(defmethod change-class :after ((instance forward-referenced-class)
                                (new prototype-class)
                                &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((implc (and (slot-boundp instance 'implementation-class)
                     (prototype-implementation-class new)))
         (implc-inst (when implc
                       (compute-class implc nil))))
    (cond
      (implc-inst
       ;; FIXME: If no IMPLC-INST -- i.e if the instance's
       ;; implementation-class is not yet defined -- need to defer
       ;; evaluation ! This may serve to require some extensions in the
       ;; compiler environment.

       #+nil
       (warn "Dispatch from CHANGE-CLASS ~S ~S to initialize ~S"
             instance new implc-inst)

       ;; NB: in PCL this may be the only way to ensure that the
       ;; direct-supeclasses slot winds up initialized from
       ;; CHANGE-CLASS (??) (FIXME: Evaluate how that's been implemented)
       (shared-initialize implc-inst (list +direct-superclasses-slot+)))
      ;; In lieu of deferred evaluation, emit a warning
      ;;
      ;; NB: In this scenario, the instance's implementation-class may
      ;; be manually initialized from outside of CHANGE-CLASS, using a
      ;; SHARED-INITIALIZE call similar to the previous.
      (t (warn "~<Unable to initilize unreachable ~
implementation class ~S for ~S~>~< during (CHANGE-CLASS ~S ~S)~>"
               implc new
               instance new)))))


;; --------------------------------------------------

(defmethod initialize-instance ((instance singleton)
                                &rest initargs &key
                                                 direct-superclasses
                                                 &allow-other-keys)
  ;; ensure that any call to MAKE-INSTANCE of a SINGLETON will produce
  ;; an object that is a subtype of the original SINGLETON

  ;; NB: In some call sequences -- such as when a singleton is
  ;; created initially from DEFCLASS, rather than CHANGE-CLASS from a
  ;; FORWARD-RERFERENCED-CLASS -- this may be "Early enough" to catch
  ;; where PCL is initially setting the direct-superclasses list for the
  ;; class definition.
  ;;
  ;; NB: See also CHANGE-CLASS, SHARED-INITIALIZE

  (let ((tgt-class (find-class 'singleton))
        (unused-class (find-class 'standard-object))
        initargs-updated)
    (declare (dynamic-extent tgt-class unused-class))
    (macrolet ((dispatch ()
                 `(cond
                    (initargs-updated
                     #+NIL
                     (warn "~<Updated Dispatch for ~S in INITIALIZE-INSTANCE ~>~
~< - DIRECT-SUPERCLASSES modified: ~S~>" instance direct-superclasses)
                     (apply #'call-next-method instance initargs))
                    (t (call-next-method)))))
      (labels ((ensure-tgt-superclass ()
                 (cond
                   (direct-superclasses
                    (unless (some (lambda (c)
                                    (find-class-in-precedence tgt-class c nil))
                                  direct-superclasses)
                      (prog1 (setf (getf initargs :direct-superclasses)
                                   (cons tgt-class
                                         (remove unused-class
                                                 direct-superclasses
                                                 :test #'eq)))
                        (setq initargs-updated t))))
                   (t (prog1 (setf (getf initargs :direct-superclasses)
                                   (cons tgt-class
                                         (remove unused-class
                                                 direct-superclasses
                                                 :test #'eq)))
                        (setq initargs-updated t))))))
        ;; NB: This does not ever check the value of SLOTS.
        ;;;
        ;; Such a check would have to be performed in a completely
        ;; implementation-specific regard, in order to detect what the
        ;; assumed DIRECT-SUPERCLASES slot would be named, for any
        ;; instance in which SLOTS would be TYPEP CONS.
        ;;
        ;; This could also, then, dispatch in a portable way singularly
        ;; for the case of (EQ SLOTS T)
        ;;
        ;; In lieu of that, this will modify SLOTS in many
        ;; instances. The modification might subsequently be ignored by
        ;; the next method.
        ;;
        ;; This method, as such, might be left undefined in the top level of
        ;; the containing source file.
        (setq direct-superclasses (ensure-tgt-superclass))

        ;; NB: Not useful if the updated value is being ignored
        ;;     by the implementation

        (dispatch)))))



;; )

(eval-when ()

  (defsingleton x-a ()
    ())

  (typep (make-instance 'x-a) 'x-a)
  ;; => T ;; consistent onto CLOS

  (subtypep (make-instance 'x-a) 'x-a)
  ;; FIXME while => NIL, T

  ;; NB/ORTHOGONAL - CLOS and MOP
  (list* (find-class nil nil)
         (let* ((nm (gentemp "Singleton-"))
                (c (make-instance 'standard-class :name nm)))
           (list (find-class nm nil)
                 c)))
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


  (find-class-in-precedence 'singleton 'x-a nil)
  ;; => #.(find-class 'X-A)

  (class-precedence-list (find-class 'x-a))


;; ...

  (defsingleton x-b (x-a)
    ;; NB: PCL will err here iff x-a is a forward referenced class
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

  (class-direct-superclasses (class-of (find-class 'x-c)))

  ;; -

  (defsingleton x-d (x-c)
    ())
  (typep (make-instance 'x-d) 'x-d)
  ;; => T ;; consistent onto CLOS

  (subtypep (make-instance 'x-d) 'x-c)
  ;; FIXME while => NIL, T
  ;; same behavior as with new DEFSINGLETON [FIXME there too]


  (class-direct-superclasses (find-class 'x-d))

  (subtypep 'x-d 'x-c)
  ;; => T, T ;; consistent onto CLOS



;; -- tests onto forward referenced classes

(defclass proto-frob-pf~2 (proto-frob-pf~1)
  ;; NB: The class proto-frob-pf~2 will be unused in this test.
  ;;
  ;;     The defclass form is evaluated once, as a trivial way to create
  ;;     a forward-referenced class
  ())

;; (class-name (class-of (find-class 'proto-frob-pf~1)))
;; => FORWARD-REFERENCED-CLASS
;; or else do not reevaluate these test forms ...

;; Now where can any portable application initialize proto-frob-pf~1
;; completely, during the following?

(defsingleton proto-frob-pf~1 (x-a)
  ())

(class-direct-superclasses (find-class 'proto-frob-pf~1))
;; ^ FIXME - needs to include SINGLETON (via ... ??)
;;

(class-direct-superclasses (class-of (find-class 'proto-frob-pf~1)))


;; -- test w/ slot definitions

(defsingleton misc-singleton ()
  ((sl-a
    :type t
    :initarg :sl-a
    :accessor misc-singleton-a
    )))

;; NB: THIS NEEDS TO BE TESTED IN A COMPILER ENVIRONMENT (??)

(class-direct-slots (find-class 'misc-singleton))
(class-slots (find-class 'misc-singleton))



)


;; --

