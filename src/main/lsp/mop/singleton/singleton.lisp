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


;; NB/TD SINGLETON Extensions => MODEL [OUTLINE]
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


(defmethod finalize-reachable ((class singleton) (seen-classes list))
  (catch 'finalize-singleton
    (macrolet ((class-seen (c seen)
                 `(find ,c ,seen :test #'eq))
               (add-seen (c seen)
                 `(setf ,seen (cons ,c ,seen)))
               (hcall (form)
                 `(handler-bind ((class-finalization-warning
                                  (lambda (c)
                                    (warn c)
                                    (throw 'finalize-singleton (values)))))
                    ;; Try to catch any call to finalize a forward-referenced
                    ;; class, such as to return immediately
                    ,form))
               (dispatch (c seen call)
                 `(progn
;;;                    #+DEBUG
                    #+NIL (warn "DISPATCH for ~S ~S - SEEN ~S : ~S"
                          (quote ,c) ,c ,seen
                          (quote ,call))
                    (unless (class-seen ,c ,seen)
                      (hcall (,call ,c ,seen))
                      ;; try this secondly
                      (add-seen ,c ,seen)))))
      (unless (class-seen class seen-classes)

        (dolist (supc (class-direct-superclasses class))
          (unless (class-seen supc seen-classes)
;;;            #+DEBUG
            #+NIL
            (warn "DISPATCH for SUPC ~S - SEEN ~S : ~S"
                  supc seen-classes 'finalize-reachable)
            (let ((seen-classes (cons class seen-classes)))
              ;; Do not destructively modify seen-classes to include CLASS yet
              (hcall (finalize-reachable class seen-classes)))
            (add-seen supc seen-classes)))

        (multiple-value-prog1
            (dispatch class seen-classes call-next-method)

          (dolist (subc (class-direct-subclasses class))
            (dispatch subc seen-classes finalize-reachable)))))))



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
  ;; Ensure that the list of direct superclasses is initialized in a
  ;; manner appropriate for an instance of the class SINGLETON
  (when (or (eq slots t)
            (and (consp slots)
                 (find +direct-superclasses-slot+ (the cons slots)
                       :test #'eq)))

    (let ((tgt-class (find-class 'singleton))
          (unused-class (find-class 'standard-object))
          (dsup (slot-value instance +direct-superclasses-slot+)))
      (declare (dynamic-extent tgt-class unused-class))

      (setq dsup (delete unused-class dsup :test #'eq))

      (unless (some (lambda (c)
                      (find-class-in-precedence tgt-class c nil))
                    dsup)
        (setq dsup (cons tgt-class dsup)))

      (setf (slot-value instance +direct-superclasses-slot+)
            dsup)))

  ;;;;
  ;;
  ;; FIXME/TBD: Update design and implementation of SINGLETON class
  ;; finalization, pursuant of reafctoring the definition of the classes
  ;; SINGLETON and PROTOTYPE-CLASS onto LTP BASE-CLASS (Metaclass)
  ;;
  ;; Refer to annotations, titled "Limitations on Design," in source file
  ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
  ;;
  ;;;;
  ;;
  ;; Also ensure that the instance's superclasses, the instance itself,
  ;; and any non-forward-referenced subclasses are finalized
  (finalize-reachable instance nil))


; Tests for Singleton Finalization - e.g
#+NIL
(eval-when ()
  ;; NOTE ALSO the updated CHANGE-CLASS method, defined below

  (macrolet ((mk (class pfx &rest direct-sup)
               `(ensure-class ;; NB: Is not MAKE-INSTANCE
                 (quote ,(gentemp
                          (concatenate 'simple-string
                                       (symbol-name pfx)
                                       #.(symbol-name '#:_nr))))
                 ,@(when direct-sup
                     (list :direct-superclasses
                           (cons 'list direct-sup))))))

    (defparameter *s1* (mk singleton s-1))


    (values
     (class-finalized-p *s1*)
     ;; => T


     (prog2 (defparameter *s2* (mk forward-referenced-class s-2 *s1*))
         (class-finalized-p *s2*))
     ;; => NIL
     ;; NB: {SBCL & PCL} does not denote a forward-referenced-class as finalized


     ;; FIXME - the definition of *S3* is now breaking, in CCL, and
     ;; breaks subsequent evaluation of this source file

     (prog2 (defparameter *s3* (mk singleton s-3 *s2* *s1*))
         (class-finalized-p *s3*))
     ;; => NIL ;; class has a forward-referenced superclass, cannot be finalized

     ;; NB: This CHANGE-CLASS call may not be portable for applications,
     ;; per [AMOP]. In the PCL MOP implementation, it can be evaluated
     ;; when *S2* is a forward-referenced-class
     (prog2 (change-class *s2* (find-class 'singleton)
                          :name (class-name *s2*)
                          :direct-superclasses (class-direct-superclasses *s2*))
         (class-finalized-p *s2*))
     ;; => T
     ;; *S2* is no longer forward referenced and should be a finalized singleton

     (class-finalized-p *s3*)
     ;; => T
     ;; *S3* should have been finalized when *S2* was finalized as a singleton

     ))

  (finalize-reachable *s3* nil)
  (class-finalized-p *s3*)
  ;; => T ;; after explicitly calling it
  (class-finalized-p *s2*)
  ;; => NIL ;; FIXME - should not be NIL. Is not NIL, in SBCL

  ;; Misc ...

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
  ;;;;
  ;;
  ;; FIXME/TBD: Update design and implementation of
  ;; SINGLETON-SLOT-DEFINITION subclasses, pursuant of reafctoring the
  ;; definition of the classes SINGLETON and PROTOTYPE-CLASS onto LTP
  ;; BASE-CLASS
  ;;
  ;; Refer to annotations, titled "Limitations on Design," in source file
  ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
  ;;
  ;;;;

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

(defun* find-class-in-precedence (the-class class &optional (errorp t))
  ;; return the first class found, of which THE-CLASS is a superclass,
  ;; or the class itself if THE-CLASS is equivalent to CLASS
  (declare (type class-designator the-class class)
           (values &optional class))
  (labels ((handle-fail ()
             (cond
               (errorp
                (error "No superclss ~S found for ~S"
                       the-class class))
               (t (values)))))
    (let ((%the-class (compute-class the-class errorp))
          (%class (compute-class class errorp)))
      (cond
        ((eq %class %the-class) (values %class))
        ((eq %class (load-time-value (find-class 't)
                                     t))
         (handle-fail))
        (t
         (dolist (c (cond
                      ((class-finalized-p %class)
                       (cdr (class-precedence-list %class)))
                      (t
                       (class-direct-superclasses %class)))
                  (handle-fail))
           (cond
             ((eq c %the-class) (return %class))
             (t (let ((found
                       (find-class-in-precedence %the-class c nil)))
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


(define-condition class-definition-error (error)
  ((class :initarg :class
          :reader class-definition-error-class)))


(define-condition slot-definition-parse-error (class-definition-error)
  ((slot :initarg :slot
         :reader slot-definition-parse-error-slot)
   (name :initarg :name
         :reader slot-definition-parse-error-name)
   (value :initarg :value
          :reader slot-definition-parse-error-value)))


(define-condition slot-definition-invalid-syntax (slot-definition-parse-error)
  ()
  (:report
   (lambda (c s)
     (let ((name (slot-definition-parse-error-slot c)))
       (format s "~<Invalid slot definition syntax~>~
~< for class ~s slot ~s :~>~< ~s~>"
               (class-definition-error-class c)
               name
               (cons name (slot-definition-parse-error-value c)))))))


(define-condition duplicate-slot-initarg (slot-definition-parse-error)
  ((previous :initarg :previous
             :reader slot-definition-parse-error-previous))
  (:report
   (lambda (c s)
     (format s "~<Duplicate slot definition initialization argument~>~
~< for class ~s slot ~s :~>~< ~s value ~s~>~< was previously ~s)~>"
             (class-definition-error-class c)
             (slot-definition-parse-error-slot c)
             (slot-definition-parse-error-name c)
             (slot-definition-parse-error-value c)
             (slot-definition-parse-error-previous c)))))


(define-condition unspported-slot-initarg (slot-definition-parse-error)
  ()
  (:report
   (lambda (c s)
     (format s "~<Unsupported slot definition initialization argument~>~
~< for class ~s slot ~s :~>~< ~s value ~s~>"
             (class-definition-error-class c)
             (slot-definition-parse-error-slot c)
             (slot-definition-parse-error-name c)
             (slot-definition-parse-error-value c)))))


;; NB: Per the particular semantics of SINGLETON class definitions,
;;     in the protocol implemented in this system: It will not be
;;     possible to use or create a forward-referenced superclass of any
;;     SINGLETON, without further extension to this protocol.


(define-condition forward-referenced-superclass (class-definition-error)
  ;; FIXME not aligned onto LTP/COMMON/MOP:CLASS-FINALIZATION-CONDITION
  ;;
  ;; Used in. ENSURE-DIRECT-SUPERCLASSES
  ((unfinalized-class
    :initarg :unfinalized-class
    :reader class-definition-error-unfinalized-class))
  (:report
   (lambda (c s)
     (format s "Cannot define class ~S with non-finalied superclass ~S"
             (class-definition-error-class c)
             (class-definition-error-unfinalized-class c)))))


(define-condition undefined-superclass (class-definition-error)
  ;; FIXME not aligned onto LTP/COMMON/MOP:CLASS-FINALIZATION-CONDITION
  ;;
  ;; Used in. ENSURE-DIRECT-SUPERCLASSES
  ((undefined-class
    :initarg :undefined-class
    :reader class-definition-error-undefined-class))
  (:report
   (lambda (c s)
     (format s "Cannot define class ~S with undefined superclass ~S"
             (class-definition-error-class c)
             (class-definition-error-undefined-class c)))))


(defmacro while (clause &body forms)
  `(block nil
     (loop (or ,clause (return))
        (progn ,@forms))))


(defun parse-defclass-slot-description (desc class)
  (declare (type cons desc) (type symbol class)
           #+(or SBCL CMUCL)
           (values cons list &optional))

  ;; NB: Return values may be destructively modified
  ;;
  ;; NB: DESC should not be destructively modified - may represent
  ;; static data, as from a compiler environment
  ;;
  ;; NB: The return value may not be, in itself, evaluated
  ;;

  ;; NB: The first return value is produced in a manner that every
  ;; element in that return value can be quoted, for processing within
  ;; a calling lexical environment.
  ;;
  ;; NB: The second return value represents a list of forms that
  ;; should be evaluated without quoting, in the calling lexical
  ;; environment. If any initform is specified, this second return
  ;; value will include :initfunction <list> for a <list>
  ;; representative of a lambda form representing the initial
  ;; initform.

  (destructuring-bind (%name &rest options) desc
    (let ((%options (copy-list options))
          name value

          ;; allow only once
          initform initfunction type
          documentation allocation

          readers writers initargs other)

      (while %options
        ;; NB This assumes OPTIONS is formatted as a property list
        (setq name (pop %options)
              value (cond
                      (options (pop %options))
                      (t (error 'slot-definition-invalid-syntax
                                :slot name :class class
                                :value options))))
        (case name
          (:initarg (setq initargs (nconc initargs (list value))))
          (:reader (setq readers (nconc readers (list value))))
          (:writer (setq writers (nconc writers (list value))))
          (:accessor
           (setq readers (nconc readers (list value))
                 writers (nconc writers (list (list 'setf value)))))
          (:initform
           ;; NB - The application will need to produce an
           ;; initialization function, such that may be evaluated
           ;; within the lexical environment calling this function.
           ;;
           ;; The following ensures that the LAMBDA form is returned
           ;; as a list type form, such that may be (but probably will
           ;; not be - FIXME) evaluated within the calling lexical
           ;; environment. Considering that the list generated with
           ;; this form is not - in fact - a function, it should not
           ;; cause any side-effects with regards to the evaluation
           ;; environment, so long as the value returned from this
           ;; function will be (FIXME) evaluated within that same
           ;; lexical environment.
           (when initform
             (error 'duplicate-slot-initarg
                    :slot %name :class class
                    :name name
                    :value value :previous initform))
           (setq initform value
                 ;; FIXME - QUOTING FOR THE INITFORM FOR EVALUATION ENVIRONMENT
                 initfunction `(lambda () ,value)))
          (:initfunction
           (error 'unsupported-slot-initarg
                  :slot %name :class class
                  :name name
                  :value value))
          (:type
           (when type
             (error 'duplicate-slot-initarg
                    :slot %name :class class
                    :name name
                    :value value :previous type))
           (setq type value))
          (:documentation
           (when documentation
             (error 'duplicate-slot-initarg
                    :slot %name :class class
                    :name name
                    :duplicate value :previous documentation))
           (setq documentation value))
          (:allocation
           (when allocation
             (error 'duplicate-slot-initarg
                    :slot %name :class class
                    :name name
                    :duplicate value :previous allocation))
           (setq allocation value))
          (t (setq other (nconc other (list (list name value)))))))
      (values  (list* :name %name (nconc
                                   (when readers (list :readers readers))
                                   (when writers (list :writers writers))
                                   (when initargs (list :initargs initargs))
                                   (when initform
                                     ;; NB: initfunction form returned
                                     ;; as a second value
                                     (list :initform initform))
                                   (when documentation
                                     (list :documentation documentation))
                                   (when allocation
                                     (list :allocation allocation))
                                   other))
               initfunction))))


(defun ensure-direct-superclasses (supers class &optional environment)
  (mapcar (lambda (c)
            (let ((%class (compute-class c nil environment)))
              (cond
                ((typep %class 'forward-referenced-class)
                 (error 'forward-referenced-superclass
                        :class class
                        :unfinalized-class c))
                (%class (values %class))
                (t
                 (error 'undefined-superclass
                        :class class
                        :undefined-class c)))))
          supers))

;; (ensure-direct-superclasses '(string vector complex) 'nope)
;; (ensure-direct-superclasses '(undef vector complex) 'nope)


(defun parse-defclass-parameter (name value)
  (declare #+(or SBCL CMUCL)
           (values list list &optional))
  ;; Polymorphic Parsing for Class Initialization Arguments
  ;;
  ;; NB: Return value may be destructively modified - as with MAPCAN
  ;;
  ;; NB: VALUE should not be destructively modified - may represent
  ;; static data, as from a compiler environment

  ;; the first return value represents values that should be quoted
  ;; before evaluation within any calling lexical environment.
  ;;
  ;; the second return value represents values that should be evaluated,
  ;; unquoted.

  ;; NB: :default-initargs => :direct-default-initargs
  ;; and more initfunctions
  (case name
    (:default-initargs
     (values nil (list :direct-default-initargs
                       (map-plist (lambda (k v)
                                    (list k (list 'quote v)
                                          `(lambda () ,v)))
                                  value))))
    (t (values (list name value) nil))))

;; (parse-defclass-parameter :default-initargs '(:a 1 :b b))
;; ^ NB: Second return value will have to be specially handled [FIXME]



(defmacro defsingleton (name (&rest superclasses)
                                slots
                        &rest params
                          &environment env)

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

  ;; FIXME: Document the :PROTOTYPE-CLASS parameter to DEFSINGLETON.
  ;;
  ;;        Note that the parameter serves to allow the calling
  ;;        environment to specify a class name for the resulting
  ;;        prototype class. In all instances of DEFSINGLETON, a class
  ;;        will be defined -- using DEFCLASS -- with that class name. A
  ;;        default value is computed, if none is specified.
  ;;
  ;; FIXME: Document the handling of the :METACLASS parameter, together
  ;;        with the direct superclass list provided to DEFSINGLETON, as
  ;;        related to the definition of the prototype metaclass.
  ;;
  ;;        In effect, the class defined by DEFSINGLETON will not be an
  ;;        instance of the class specified in the :METACLASS parameter,
  ;;        but will be an instance of a subtype of that class.

  ;; NB: As the singleton metaclass definition is now being handled
  ;;     via some extensions onto ALLOCATE-INSTANCE, SHARED-INITIALIZE,
  ;;     and ENSURE-CLASS, this macro will no longer expand to a
  ;;     DEFCLASS form.

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

           (compute-prototype-class-spec (params)
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
                    (values (cdr name-prop) params-adj)))
                 (t (values nil params))))))

    (multiple-value-bind (proto-spec %params)
        (compute-prototype-class-spec params)

      (with-symbols (proto-meta direct-meta base-class %superclasses
                                the-class)

        (multiple-value-bind (user-metaclass %params)
            (compute-user-metaclass %params)

          ;; NB: Using an implcit PROTOTYPE-METACLASS is-a SINGLETON here.

          `(progn

             ;; NB: This implementation, in effect, prevents any usage
             ;; of forward-referenced classes in the SUPERCLASSES list

             (let* ((,base-class (find-class 'singleton))
                    ;; ^ FIXME define +SINGLETON-BASE-CLASS+
                    ;;
                    ;; see also ALLOCATE-INSTANCE, below
                    ;;
                    (,direct-meta ,(cond
                                     (user-metaclass
                                      `(compute-class ,@user-metaclass))
                                     (t base-class)))
                    (,%superclasses
                     (ensure-direct-superclasses (quote (,@superclasses))
                                                 (quote ,name)
                                                 ,env))
                    (,proto-meta
                     (ensure-prototype-metaclass (or ,proto-spec
                                                     (ensure-prototype-metaclass-name
                                                      ,base-class (quote ,name)))
                      ,direct-meta ,base-class ,%superclasses))
                    (,the-class
                     (#+NIL apply
                            ensure-class
                      (quote ,name)
                      :direct-superclasses ,%superclasses
                      :metaclass ,proto-meta

                      :direct-slots
                      (list
                       ,@(mapcar #'(lambda (spec)
                                     (multiple-value-bind (parsed initfn)
                                         (parse-defclass-slot-description spec name)
                                       (cons 'list
                                             (nconc (mapcar #'(lambda (literal)
                                                                (list 'quote literal))
                                                            parsed)
                                                    (when initfn
                                                      (list :initfunction initfn))))))
                                 slots))

                      ;; &REST
                         ,@(mapcan #'(lambda (spec)
                                     (destructuring-bind (param . pvalue) spec
                                       (multiple-value-bind (to-quote eval)
                                           (parse-defclass-parameter param pvalue)
                                         (nconc (mapcar #'(lambda (literal)
                                                            (list 'quote literal))
                                                        to-quote)
                                                (apply #'nconc
                                                (map-plist
                                                 #'(lambda (name form)
                                                     (case name
                                                       (:direct-default-initargs
                                                        `(#+NIL list
                                                          ,name
                                                          (list ,@(mapcar #'(lambda (f)
                                                                              (cons 'list f))
                                                                          form))))
                                                       (t (list name form))))
                                                 eval)
                                                )))))
                                 %params)
                         )))


               ;; NB/DOCUMENTATION (and PCL) - NOTE THE FOLLOWING
               (shared-initialize ,the-class (list +direct-superclasses-slot+))
               ;; ^ FIXME reimplement that behavior, onto ENSURE-CLASS (SINGLETON)

               (setf (%prototype-implementation-class ,proto-meta)
                     ,the-class)
               (values ,the-class))))))))

;; --

;; FIXME: Test DEFSINGLETON with a superclass not being of a metatype
;; SINGLETON, but such that a direct superclass of that class is of
;; metatype SINGLETON

#+NIL
(eval-when ()

  ;; TO DO - TEST DEFSINGLETON w/ :default-intiargs, environment
  ;; TO DO - TEST DEFSINGLETON w/ slotdef initforms, environment

  (defclass frob-c ()
    ())


  (defsingleton frob-s-1 (frob-c)
    ())

  (class-finalized-p (find-class 'frob-s-1))
  ;; => T [SBCL] [CCL]

  (class-direct-superclasses (find-class 'frob-s-1))
  ;; =>( #.(... SINGLETON) #.(... FROB-C))

  (typep (find-class 'frob-s-1) 'frob-c)
  ;; => T

  (subtypep (find-class 'frob-s-1) 'frob-c)
  ;; => T, T

  (typep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; => T

  ;; OK ...

  ;; NB - Errors seen w/ SUBTYPEP for non-finalized classes [CCL]

  ;; (class-of (find-class 'frob-s-1))
  ;; (class-direct-superclasses (class-of (find-class 'frob-s-1)))
  ;; => (#<SINGLETON PROTOTYPE-CLASS>
  ;;     #<STANDARD-CLASS SINGLETON>
  ;;     #<STANDARD-CLASS FROB-C>)
  ;;
  ;; This, now....
  ;; (subtypep (find-class 'frob-s-1) 'standard-class)
  ;;
  ;; FIXME => NIL, T [CCL] (??!!?) ;; IFF FROB-S-1 IS NOT FINALIZED
  ;; ^ by side-effect, this breaks some of the following tests & protocol
  ;;
  ;; OK => T, T [SBCL]


  ;; (subtypep (find-class 'singleton) 'standard-class)
  ;; => T, T [CCL]...
  ;;
  ;; (subtypep (find-class 'frob-s-1) 'singleton)
  ;; => NIL, T (!?) [CCL] ;; IFF FROB-S-1 IS NOT FINALIZED
  ;; => T, T [SBCL]

  ;; (find (find-class 'singleton) (class-precedence-list (find-class 'frob-s-1)))

  ;; FIXME - CCL - IF FROB-S-1 HAS NOT BEEN FINALIZED
  ;; (class-precedence-list (find-class 'frob-s-1))
  ;; => (#<FROB-S-1!SINGLETON FROB-S-1> #<STANDARD-CLASS FROB-C> #<STANDARD-CLASS STANDARD-OBJECT> #<BUILT-IN-CLASS T>)
  ;; ^ no #<SINGLETON> even while ...
  ;;
  ;; (class-direct-superclasses (find-class 'frob-s-1))
  ;; => (#<SINGLETON> #<FROB-C>)


  ;; (finalize-inheritance (find-class 'frob-s-1))

  ;; (subtypep (class-of (find-class 'frob-s-1)) 'standard-class)
  ;; => T, T ;; of course
  ;;
  ;; (typep (class-of (find-class 'frob-s-1)) 'standard-class)
  ;; => T ;; of course

  ;; (subtypep (find-class 'standard-class) 'standard-object)
  ;; => T, T [CCL] [SBCL] ...



  (subtypep (make-instance (find-class 'frob-s-1)) 'frob-c)
  ;; NB => NIL, T ;; NB: BREAKS in CCL IF FROB-S-1 NOT FINALIZEDC
  ;;
  ;; FIXME: Consider inheriting the direct superclasses from the
  ;; metaclass, for any SINGLETON instance
  ;;
  (subtypep (make-instance (find-class 'frob-s-1)
                           :direct-superclasses (list (find-class 'frob-s-1)))
            'frob-c)
  ;; => T, T

  #+NIL ;; NB: the :DIRECT-SUPERCLASSES sequence may be ignored, "By now" ....
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

  ;; --

  (macroexpand-1 (quote
  (defsingleton singleton-1-2 ()
    ((sl-a
      :initarg :sl-a
      :initform "the-sl-a")
     (sl-b
      :initarg :sl-b))
    ;; NB: :DEFAULT-INITARGS should be handled specially in SINGLETON [FIXME]
    (:default-initargs :sl-a *default-pathname-defaults* :sl-b (quote nil)))

  ;; (class-direct-default-initargs (find-class 'singleton-1-2))
  ;;
  ;; (slot-value (make-instance 'singleton-1-2) 'sl-a)
  ;; => <varies lexically>
  ;;
  ;; (slot-value (make-instance 'singleton-1-2) 'sl-b)
  ;; => NIL

  ))

)

#+TBD
(eval-when ()


(macroexpand-1 (quote
(defsingleton singleton-1-1 ()
  ((sl-1
    :initform "the-sl-1")
   (sl-b)))
))

(class-direct-superclasses (find-class 'singleton-1-1))
;; NB: ^ should always include SINGLETON

(class-of (find-class 'singleton-1-1))

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
;; FIXME [CCL] - LOOP (??)

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
;; => NIL, T

(subtypep (make-instance 'singleton-2-2) 'singleton-1-1)
;; => NIL, T

(subtypep (make-instance 'singleton-2-2
                         :direct-superclasses
                         (list (find-class 'singleton-2-2)))
          'singleton-1-1)
;; => T, T

(typep (make-instance 'singleton-2-2) 'singleton-1-1)
;; => T
;; OK !


;; NB: To produce a suclass C2 of a class C1, such that C2 is both an
;; instance typep C1 and a subtype of C1 ...
;;
;; ... one may define MAKE-INSTANCE => CREATE-IMPLICIT-SUBCLASS
;; via ALLOCATE-INSTANCE

)


;; --------------------------------------------------


(defmethod change-class :after ((instance forward-referenced-class)
                                (new prototype-class)
                                &rest initargs &key &allow-other-keys)
  ;;;;
  ;;
  ;; FIXME/TBD: Update design and implementation of SINGLETON class
  ;; finalization, pursuant of reafctoring the definition of the classes
  ;; SINGLETON and PROTOTYPE-CLASS onto LTP BASE-CLASS (Metaclass)
  ;;
  ;; Refer to annotations, titled "Limitations on Design," in source file
  ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
  ;;
  ;;;;

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
       (shared-initialize implc-inst (list +direct-superclasses-slot+))
       (finalize-reachable implc-inst nil)
       (finalize-reachable instance (list implc-inst)))
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

  ;; NB: This method serves to ensure that SINGLETON should appear in
  ;; the class precedence list for INSTANCE. If none of the
  ;; direct-superclasses is a subclass of SINGLETON, the class SINGLETON
  ;; will be added to the head of the direct superclasses list.  This
  ;; method also serve to ensure that any instance of the class
  ;; STANDARD-OBJECT is removed from the initial direct superclasses
  ;; list.

  ;; NB: In some call sequences -- such as when a singleton is created
  ;; initially from DEFCLASS, rather than CHANGE-CLASS from a
  ;; FORWARD-RERFERENCED-CLASS -- this may be "Early enough" to catch
  ;; where PCL is initially setting the direct-superclasses list for the
  ;; class definition.  NB: See also CHANGE-CLASS, SHARED-INITIALIZE


  ;; FIXME: Also ensure that any call to MAKE-INSTANCE of a SINGLETON
  ;; will produce an object that is a subtype of the original
  ;; SINGLETON. This may be approached, somewhat portably, with
  ;; ALLOCATE-INSTANCE. Such a methodology  may serve to require
  ;; that a new  class will be initialized within some calls to
  ;; ALLOCATE-INSTANCE - rather than within a top-level DEFCLASS form,
  ;; such that would be processed by the implementation compiler.
  ;;
  ;; In such a methodology, there may be an effect produced such that
  ;; the class of a SINGLETON created with MAKE-INSTANCE would a
  ;; subclass of, but not equivalent to the class provided to
  ;; MAKE-INSTANCE
  ;;
  ;; To an effect
  ;;   (subtypep (make-instance the-singleton) the-singleton) => T, T
  ;;

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

        ;;;;
        ;;
        ;; FIXME/TBD: Update design and implementation of SINGLETON class
        ;; finalization, pursuant of reafctoring the definition of the classes
        ;; SINGLETON and PROTOTYPE-CLASS onto LTP BASE-CLASS (Metaclass)
        ;;
        ;; Refer to annotations, titled "Limitations on Design," in source file
        ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
        ;;
        ;;;;

        (setq direct-superclasses (ensure-tgt-superclass))

        ;; NB: Not useful, if the updated value is being ignored
        ;;     by the implementation. See also SHARED-INITIALIZE

        (dispatch)))))



;; )

#+NIL
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

  (subtypep (class-of (make-instance 'x-b)) 'x-a)
  ;; => T, T
  ;; consistent onto CLOS
  ;;
  ;; approximately equivalent:
  (subtypep (find-class 'x-b) 'x-a)
  ;; => T, T
  ;; consistent onto CLOS


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

;; NB: The following was originally defined as a prototype
;; onto two classes SINGLETON* and SINGLETON**. As such, the source code
;; was located at end-of-file.
;;
;; This should be evaluated before any calls to the updated DEFSINGLETON macro


(defgeneric class-prototype-metaclass (class)
  (:method ((class standard-class))
    (load-time-value (find-class 'prototype-class)
                     t)))



(defgeneric ensure-prototype-metaclass (proto use-metaclass for-class
                                        direct-superclasses)
  ;;;;
  ;;
  ;; FIXME/TBD: Update design and implementation of SINGLETON class
  ;; finalization, pursuant of reafctoring the definition of the classes
  ;; SINGLETON and PROTOTYPE-CLASS onto LTP BASE-CLASS (Metaclass)
  ;;
  ;; Refer to annotations, titled "Limitations on Design," in source file
  ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
  ;;
  ;;;;

  (:method ((proto singleton) (use-metaclass standard-class)
            (for-class standard-class) ;; may be #<SINGLETON>
            (direct-superclasses list))
    ;; (declare (ignore use-metaclass for-class direct-superclasses))
    ;; FIXME - note the FIND-CLASS call, below, similarly
    ;; (values proto))

    ;; NB - a trivial hack to ensure that the PROTO class matches per
    ;; the  protocol implemented for a newly defined prototype-metaclass
    (ensure-prototype-metaclass (class-name proto)
                                use-metaclass for-class
                                (remove proto direct-superclasses
                                        :test #'eq)))


  (:method ((proto null) (use-metaclass standard-class)
            (for-class singleton)
            (direct-superclasses list))
    (error "Invalid prototype metaclass name for ~s: ~s" proto
           for-class))


  (:method ((proto symbol) (use-metaclass standard-class)
            (for-class standard-class) ;; may be #<SINGLETON>
            (direct-superclasses list))

    (or (find-class proto nil) ;; NB - CLASS REUSE
        ;; FIXME - only reuse the class when it conforms to the
        ;; following implementation pattern - else generate a new
        ;; prototype class name and dispatch to the following

        ;; FIXME - SORT-CLASSES needs an update
        (labels ((find-class-in-superclasses (c supers)
                   (some #'(lambda (sc)
                             (find-class-in-precedence c sc nil))
                         supers))
                 (guarded-add (c supers)
                   (cond
                     ((find-class-in-superclasses c supers)
                      (values supers))
                     (t (cons c supers))))
                 (sort-classes (proto-meta for-c use-meta direct-supers)
                   ;; NB - does not add FOR-C if it's EQ to USE-META
                   ;;
                   ;; Similarly, does not add PROTO-META if it's EQ to USE-META
                   ;;
                   ;; ... or if  appears in the class precedence list
                   ;; of any class in DIRECT-SUPERS
                   ;;
                   ;; NB: This does not sort the result per any added classes
                   ;;
                   ;; FIXME This fails to add #<SINGLETON> when evaluated
                   ;; w/ CCL
                   (guarded-add proto-meta
                                (guarded-add for-c
                                             (guarded-add use-meta
                                                          direct-supers)))))
          (let ((proto-metaclass (class-prototype-metaclass for-class))
                (%direct-superclasses
                 ;; TBD - wrap the COMPUTE-CLASS call with something to
                 ;; signal a FORWARD-REFERENCED-SUPRECLASS error
                 ;; onto FOR-CLASS, for any forward-referenced class
                 ;; in direct-superclasses
                 (mapcar #'compute-class direct-superclasses)))
            (ensure-class proto
                          :metaclass  proto-metaclass
                          :name proto

                          :direct-superclasses
                          ;; FIXME : Trivial superclass sorting
                          (sort-classes proto-metaclass
                                        for-class
                                        use-metaclass
                                        %direct-superclasses)

                          :implementation-class for-class))))))


(defmethod shared-initialize  ((instance singleton) slots
                              &rest initargs
                                &key
                                  (ensure-prototype nil esp)
                                  &allow-other-keys)
  (declare (ignore ensure-prototype))
  ;; Remove (and not delete) initargs not used in the class instance
  ;;
  ;; NB: This method's definition, itself, may have a side-effect of
  ;; validating the initarg :ENSURE-PROTOTYPE during ENSURE-CLASS and similar
  ;;
  (let (initargs-updated)
    (macrolet ((remf* (whence name)
                 (with-symbols (%whence)
                   `(let ((,%whence (copy-list ,whence)))
                      (remf ,%whence ,name)
                      (values ,%whence))))
                 (dispatch ()
                 `(cond
                    (initargs-updated
                     (apply #'call-next-method instance slots initargs))
                    (t (call-next-method)))))
      (when esp
        (setq initargs (remf* initargs :ensure-prototype)
              initargs-updated t))

      (dispatch))))



(defgeneric ensure-prototype-metaclass-name (for-metaclass class-name)
  (:method ((for-metaclass standard-class) (class-name symbol))
    (intern
     (concatenate 'simple-string (symbol-name class-name)
                  "!" (symbol-name (class-name for-metaclass))))))



(defmethod allocate-instance ((class singleton)
                              &rest initargs)
  (cond
    ((or (getf initargs :prototype-class)
         (getf initargs :ensure-prototype))
     ;; ^ NB: Prevent that this protocol would be, in effect, active for
     ;; definitions of SINGLETON classes outside of DEFSINGLETON and similar.


     ;; FIXME - Consider inheriting direct superclasses (filtered) from
     ;; the metaclass

     (labels ((mk-default-name (class)
                (make-symbol
                 (concatenate 'simple-string
                              #.(symbol-name (quote #:uninterned-))
                              (symbol-name (class-name class))))))
       (let* ((name (or (getf initargs :name (mk-default-name class))))
              (proto-spec (getf initargs :prototype-class
                                (ensure-prototype-metaclass-name class name)))
              (metaclass (compute-class
                          (getf initargs :metaclass
                                (load-time-value (find-class 'singleton)
                                                 t))))
              (direct-supers (getf initargs :direct-superclasses))
              (proto-metaclass
               (ensure-prototype-metaclass proto-spec metaclass class
                                           direct-supers)))

         ;;;;
         ;;
         ;; FIXME/TBD: Update design and implementation of SINGLETON class
         ;; finalization, pursuant of reafctoring the definition of the classes
         ;; SINGLETON and PROTOTYPE-CLASS onto LTP BASE-CLASS (Metaclass)
         ;;
         ;; Refer to annotations, titled "Limitations on Design," in source file
         ;; ltp-main:src/main/lsp/base-class/fdef-acc.lisp
         ;;
         ;;;;


         #+NIL
         (warn "ALLOCATE-INSTANCE ~s as instance of ~s" class proto-metaclass)

         (let ((%initargs (copy-list initargs)))
           (remf %initargs :ensure-prototype)
           (remf %initargs :prototype-class)
           (apply #'allocate-instance proto-metaclass %initargs)))))
    (t (call-next-method))))


#+NIL
(eval-when ()

  (let ((s (find-class 'prototype-class)))
    (defparameter *the-s*
      (ensure-class (gentemp "S-") :metaclass s
                     :prototype-class (gensym "S-PROTO-")
                     :direct-superclasses (list s)
                     ))
    (finalize-inheritance *the-s*)
    (values #+NIL (typep *the-s* s)
            *the-s*))

  ;; (class-of *the-s*)

  ;; (class-of (class-of *the-s*))


  ;; (class-direct-superclasses (class-of *the-s*))

  ;; (class-direct-subclasses (find-class 'prototype-class))

  ;; NB ! QA for the updated ALLOCATE-INSTANCE method
  ;; (class-of (class-prototype (find-class 'singleton)))
  ;; ^ should be the class SINGLETON


  (eq (class-of (class-prototype *the-s*) ) *the-s*)
  ;; => T

  (eq (class-of (class-prototype (class-of *the-s*)))
      (class-of *the-s*))
  ;; => T

  (subtypep *the-s* 'singleton)
  ;; => T, T

  (typep *the-s* 'singleton)
  ;; T

  ;; and of course (per CLOS unadorned) ...
  (typep (make-instance *the-s*) 'singleton)
  ;; T


  ;; NOW THIS NEEDS DOCUMENTATION

  ;; &MISC
  (subtypep (make-instance *the-s*) 'singleton)
  ;; T, T ;; per CLOS
  (subtypep (ensure-class (gensym "THE-S") :metaclass *the-s*) 'singleton)
  ;; T, T
  (subtypep (make-instance *the-s*) 'singleton)
  ;; T, T


  ;; --

  (let ((s (find-class 'prototype-class)))
    (defparameter *the-s-2*
      (make-instance s :name (gentemp "S2-")
                     ;; NB - make-instance with :prototype-class class
                     ;;
                     ;; Ensuring prototype-class reuse here - trivial test
                     :prototype-class (find-class 'prototype-class)
                     :direct-superclasses (list s)
                     :ensure-prototype t
                     :metaclass s
                     ))
    (finalize-inheritance *the-s-2*)
    (values #+NIL (typep *the-s* s)
            (symbol-value '*the-s-2*)))

  ;; (typep *the-s-2* 'singleton)
  ;; => NIL ;; consequent of the user-specified metaclass


  ;; --

  (let ((s (find-class 'prototype-class)))
    (defparameter *the-s-3*
      (make-instance s :name (gentemp "S3-")
                     ;; NB test prototype-class name initialization
                     :direct-superclasses (list s)
                     :ensure-prototype t
                     :metaclass s
                     ))
    (values #+NIL (typep *the-s* s)
            (symbol-value '*the-s-3*)))


  ;; -- test with non-singleton user-specified superclasses

  (defclass obj-a ()
    ())

  (let ((s (find-class 'prototype-class)))
    (defparameter *the-s-4*
      (ensure-class (gentemp "S4-") :metaclass s
                     :direct-superclasses (list (find-class 'obj-a))
                     :ensure-prototype t
                     ))
    ;; (finalize-inheritance *the-s-4*)
    (values #+NIL (typep *the-s* s)
            (symbol-value '*the-s-4*)))


  #+NIL
  (make-instance (find-class 'prototype-class)
                 :ensure-prototype t
                 :metaclass (find-class 'singleton))


  (make-instance (find-class 'prototype-class)
                 ;; :ensure-prototype t
                 :metaclass (find-class 'singleton))
  ;; ^ No loop, but not using the correct metaclass semantics

  (class-of *the-s-4*)
  ;; => #<PROTOTYPE-CLASS>
  ;;
  (typep *the-s-4* 'obj-a)
  ;; => T
  ;;
  (subtypep *the-s-4* 'obj-a)
  ;; => T, T
  ;;
  (typep (make-instance *the-s-4*) 'obj-a)
  ;; => T
  ;;
  ;; ... which is pretty much the point of this - NB [DOCUMENTATION]
  ;; ... in which it's more or less like DEFSINGLETON but implemented
  ;;     via ALLOCATE-INSTANCE

  ;; nb - consequent of ENSURE-CLASS
  (find-class (class-name *the-s-4*) nil)
  ;; => <CLASS>
  ;;
  (find-class (class-name (class-of *the-s-4*)) nil)
  ;; => <CLASS>

  (class-direct-superclasses *the-s-4*)


  (subtypep (make-instance *the-s-4*) 'obj-a)
  ;; NB => NIL, T ;; note no :DIRECT-SUPERCLASSES specified to the instance

  (subtypep *the-s-4* 'obj-a)
  ;; => T

  ;; (describe (find-class 'obj-a))

  ;; NB
  (class-of (class-of *the-s-4*))
  ;; => #<SINGLETON PROTOTYPE-CLASS>


  (subtypep (make-instance *the-s-4* :direct-superclasses (list *the-s-4*))
            'obj-a)
  ;; => T, T

  ;; --


  ;; FIXME - Test with user-specified metaclass

)
