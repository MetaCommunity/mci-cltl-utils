
(in-package #:cl-user)


;; cf. CORBA Naming Service 1.3 (2004)


#|

 Commentary: FELDSPAR IDL mapping for Common Lisp

 The following source code proposes a representation for IDL in Common
 Lisp, alternate to the conventional IDL to Lisp mapping.

 TO DO:
 * Implement, Test, and Document the following prototype forms

   * "Case collapsing" for names: CORBA specifications indicate that
     IDL names must be compared in a case sensitive manner, but that
     it constitutes a name collision when names are equivalent without
     regards to case conversion. 

     In order to provide a facility for memoization of string values
     in a compatible string comparison function, this implementation
     provides the `IDL-NAME-FLATTENED` slot on the class
     `IDL-DEFINITION`. The value of IDL-NAME-FLATTENED slot is
     generated automatically as a STRING-UPCASE version of the
     :IDL-NAME initialization argument value, during SHARED-INITIALIZE
     of an IDL-DEFINITION (TO DO: Updated flatened name value during
     SETF SLOT-VALUE IDL-DEFINITION IDL-NAME)

   * Sidebar: Slot-value locking, Modal (read/write) slot-value
     locking, and WITH-SLOT-LOCKED macro implementations

     Ideally, in a multithreaded programming environment, it would
     "Just work out" that an object's slot values would  never be
     concurrently read and modified in separate threads, in any manner
     that would produce an inconsistency between values respectively
     read and written in separate threads - as whether for 'atomic' slot
     values or slot values backed with a sequence type 'backing
     store'. 

     Hypothetically, a CORBA application implementing a dynamic
     invocation interface (DII) may endeavor to set a CORBA
     interface's 'name' value from a thread A, in such a manner that
     would produce a multiprocessing race condition if the same
     interface's 'name' value is being 'read' in a thread B.

     Hypothetically, furthermore, a CORBA name service may endeavor to
     resolve a name M avaialble in a naming context N in a thread A,
     while the same name M is being removed from N in a thread B,
     again such that would produduce a multiprocessing race
     condition. Such conditions at least logically interfere with a
     deterministic model of a program's execution. In a practical
     regards, such conditions may ever serve to present a practial
     dificulty wiht regards to profiling and debugging of a production
     system. 

     For any single object that may be accessible within multiple
     threads, ostensibly any single slot value of the object may be
     accessed likewise from multiple threads - as regardless of the
     slot definition allocation for the slot value. 

     As an extension onto the metaobject protocol, a methodology may
     be defined in which any single slot or set of slots may be locked
     from a single thread - whether locked in a manner as to allow
     concurrent reads but block conncurrent writes, or locked
     exclusively, as in a manner to block conccurent reads and writes
     - as regardless of the allocation of the slot's definition and
     slot value. 

     A manner of consideration may be developed as about the
     allocation of the 'lock' objects for such an extension. For
     instance: 

        * if a slot definition is 'class' allocated, any lock applied
          for accessing the slot's value may be stored in reference
          to the class of the slot's allocation - whether stored in a
          slot value on the class definition itself, or optinally
          stored in a property value defined onto the class' symbolic
          class name.

        * if a slot definition is 'instance' allocated, any lock
          applied for accessing the slot's value should be stored in
          reference to any single instance in which the slot would be
          accessed

        * for instance allocated slots, for each slot that is defined
          as to be "lockable", the slot definition for the respective
          slot may be defined as to contain an adjustable array of
          instances and corresponding, implementation-specific lock
          objects. For each instance whose slot value must be locked,
          for a duration - whether locked in a 'read' or exclusive
          'write' mode - the instance may be added to the array,
          together with a freshly allocated, implementation-specific
          'lock' object - the 'lock' being initialized originally for
          a 'read' or 'write' modality in  the duration, onto the 
          respective instance. Effectively, the instance containing
          the slot definition would provide the key onto the table as
          such - such that may be compared with a direct EQ
          comparison, without immediate reference to a hashing
          algorithm.

        * for class allocated slots, for each slot that is defined as
          to be 'lockable', the class definition itself may be
          modified so as to contain a "cache" as that would store an
          implementation-specific lock object for any duration in
          which the respective class allocated slot is being
          accessed. 

        * Whether for class allocated or instance allocated slot value
          locking, a "locks pool" may be developed - perhaps, in a
          manner functionally analogous to a network service thread 
          pool, but producing implementation-specific 'lock' objects
          rather than implementation-specific 'thread' objects, as,
          objects allocated in the respective lock pooling
          implementation. For most instances, it may be possible  
          to prevent that a new lock object would have to be created
          in the respective host operating system kernel, for each
          instance in which a lock must be allocated as so.

        * For instance-allocated "Lockable slots", in a MOP
          implementation, ostensibly a new set of direct and efective
          slot definition classes may be deined, togehter with a set
          of methods specialized such as to lock an instance's slot
          value for concurrent "read" from within
          SLOT-VALUE-USING-CLASS and to lock the slot value against
          concurrent "read" or "Write" from within (SETF
          SLOT-VALUE-USING-CLASS). Of course, each respective
          lock-holding form should unwind appropriately on any event
          of an unexpected situation - releasing any lock held, in
          such event.

        * Slot value "Lockability" may be specified by the systems
          programmer, in definitions of each individual
          class. Ostensibly, any slot whose value may be logically
          accessed from concurrent threads, within an application,
          should be defined as to be 'lockable', with the
          implemenation then providing an automatic 'locking' during
          slot value access.

        * Ostensibly, a slot value locking protocol may find an
          application in data services providing concurrent access to
          individual data objects in Common Lisp.

   * Name equivalence onto definition kinds - symbols, strings, and
     IDL string tranformation to/from symbol names - refer to existing
     binding specs.

   * Implementation onto existing CORBA IDL "basic types"

       * IDL string => CL:SIMPLE-BASE-STRING (if all of ISO-8559-1 is
         in the SIMPLE-BASE-STRING value space, else => SIPLE-STRING)
         unless application deems other than 'simple strings'
       * IDL wstring => CL:SIMPLE-STRING unless application deems otherwise

   * Integrate with existing CLORB IDL Parser

   * Integrate with CORBA DII IDL

   * Integrate with any existing DII services in CLORB

  * "Publish"

  * "Next Milestone"

|#

#| NOTES: FELDSPAR Name Resolver Implementation

 * TO DO: Design for platform-agnostic integration w/ POSIX interfaces
   onto 1) networking services at OSI Layers 1..7, 2) user credential
   services available on-host; 3) implementationd after POSIX 1003.1e;
   4) filesystem services ... referencing initially a FreeBSD 10.2
   kernel and userspace utilities

    * CosFileTransfer/EntryName (wstring) and CosFileTransfer/EntryPath
      (sequence of EntryName), CosFileTransfer/DirEntrySeq  (used in
      DirEntryIterator) ... woul be accessed via interfaces defined in
      the CosFileTransfer module, in an implementation of the same. It
      would provie a logical extension onto CosFileTransfer, if to
      provide an alternate file name resolution service via CosNaming
      - moreover would serve to require some manner of object casting,
      for application of any object reutrned by the 'resolve' function
      in CosNaing, if applied onto objects defined principally in a
      CosFileTransfer interface.

* In the Common Lisp API, define procedures for automatically deriving
  a name component's 'name kind' value from the class of the object to
  which the name compinent resolves

* Case study: Resolving name components w/ same 'name' but differing
  'kind' in a single naming context

* Case study: Naming context as a VFS directory service (filename type
  as name 'kind')

* Case study: Naming contexts and name objects onto an interface to an
  Augeaus configuration editor service

|#


#+:FELDSPAR
(defclass idl-definition-kind (standard-class)
  ())


#+:FELDSPAR
(defgeneric definition-idl-name (definition))
#+:FELDSPAR
(defgeneric (setf definition-idl-name) (new-name definition))


#+:FELDSPAR
(defclass definition-container ()
  ())

#+:FELDSPAR
(declaim (type *definition* definition-container))

#+:FELDSPAR
(defvar *definition* (make-root-definition-container))

#+:FELDSPAR
(deftype definition-name ()
  '(or simple-string symbol))

#+:FELDSPAR
(defun definition-name-equal (n1 n2)
  (declare (type definition-name n1 n2)
           (values boolean &optional))
  
  )

#+:FELDSPAR
(defun find-definition-using-name (name container &optional (errorp t))
  (declare (type idl-name name)
           (values (or definition-container null) &optional))
  (let ((inst (find name
                    (container-definitions *container*)
                    :key #'definition-idl-name
                    :test #'definition-name-equal
                    )))
    (cond
      (inst (values inst))
      (errorp
       (error 'the-not-found-error :name name :container container))
      (t (values nil)))))


#+:FELDSPAR
(defmacro in-definition (name)
  (let ((%inst (gensym "%inst-"))
        (%name (gensym "%name-")))
    `(let* ((,%name (quote ,name))
            (,%inst (find-definition-using-name ,%name *definition*)))
       (setq *sontainer* ,%inst))))

#+:FELDSPAR
(defgeneric module-name-prefix (module))
#+:FELDSPAR
(defgeneric (setf module-name-prefix) (new-prefix module))

#+:FELDSPAR
(defclass idl-definition ()
  ((idl-name
    :initarg :idl-name
    ;; FIXME: VALIDATE VALUE SYNTAX DURING INITIALIZE/SET
    :type simple-string
    :accessor definition-idl-name)
   (idl-name-flattened
    ;; "Case collapsed" form of IDL-NAME
    :initarg :idl-name-adjused
    :type simple-string
    :accessir definition-idl-name-flattened)
   (container
    :type definition-container
    :accessor definition-container)
   (version
    ;; cf. "#prgma version"
    :initarg :version
    ;; FIXME: VALIDATE VALUE SYNTAX DURING INITIALIZE/SET
    :type simple-base-string
    :accessor definition-version)
   (repository-id
    ;; FIXME: VALIDATE VALUE SYNTAX DURING SET
    ;; cf. "#pragma id" if explicitly set,
    ;; otherwise computed from container, version
    :initarg :repository-id
    :accessor definition-repository-id
    )))


#+:FELDSPAR
(defun write-default-repository-id (instance stream)
  (declare ((type idl-definition instance)
            (type stream stream)))
  (cond
    ((slot-boundp instance 'container)
     (write-repository-id (definition-container instance)
                          stream)
     (write-char #\/ stream))
    (t
     (write-string "IDL:" stream)
     (when (slot-boundp instance 'prefix)
       (write-string (module-name-prefix instance)
                     stream)
       (write-char #\/ stream))))
  (write-string (definition-idl-name instance) stream)
  (when (slot-boundp instance 'version)
    (write-char #\: stream)
    (write-string (the simple-base-sring (definition-version instance))
                  stream)))

#+:FELDSPAR
(defmethod shared-initialize :around ((instance idl-definition)
                                      slots &rest initargs
                                      &key idl-name &allow-other-keys)
  (let (args-updated-p)
    
    (when idl-name
      
      (let ((simple (simplify-string idl-name)))
        (unless (eq simple idl-name)
          (setf (getf initargs :idl-name) simple)
          (setq args-updated-p t)))

      (when (or (eq slots t)
                (and (consp slots)
                     (member 'idl-name-flattened (the cons slots)
                             :test #'eq)))
        (setf (getf initargs :idl-name-flattened)
              (string-upcase (the simple-string idl-name)))
        (setq arg-updated-p t)))
    
    (when (next-method-p)
      (cond
        (args-updated-p
         (apply #'call-next-method instance slots initargs))
        (t (call-next-method))))
      
    (when (and (not (boundp instance 'repository-id))
               (or (eq slots t)
                   (and (consp slots)
                        (member 'repository-id (the cons slots)
                                :test #'eq))))
      (with-slot-locked (instance repository-id :write)
        ;; complex initform : repository-id -
        ;; FIXME: FOR CLASSES, DO AFTER FINALIZE-CLASS ?
        (let ((str (make-string-output-stream :element-type 'base-char)))
          (write-default-repository-id instance stream)
          (setf (definition-repository-id instance)
                (get-output-stream-string str)))))))


#+:FELDSPAR
(defun write-repository-id (instance stream)
  (declare ((type idl-definition instance)
            (type stream stream)))
  (cond
    ((slot-boundp instance 'repository-id)
     (write-string (definition-repository-id instance)
                   stream))
    ;; should not ever be reached, except during instance init:
    (t (write-default-repository-id instance stream))))


#+:FELDSPAR
(defclass module-definition (definition-container idl-definition)
  ((name-prefix
    ;; cf. "#pragma prefix"
    :initarg :prefix
    :accessor module-name-prefix))
  (:idl "module")
  (:metaclass idl-definition-kind))


#+:FELDSPAR
(defclass idl-structural-definition (idl-definition)
  ;; protocol class, i.e. mixin
  (#+TO-DO
   (members ...)
   ;; TO DO: Validate member add/remove
   ;; * no module definitions in interface definitions
   ;; * no method definitions in module definitions
   ))


#+:FELDSPAR
(defclass struct-definition (idl-structural-definition)
  ()
  (:idl "struct")
  (:metaclass idl-definition-kind))

#+:FELDSPAR
(defclass interface-definition (definition-container idl-structural-definition)
  ()
  (:idl "interface")
  (:metaclass idl-definition-kind)
  )

#+:FELDSPAR
(defclass type-definition (idl-definition)
  ;: TO DO: Define each core (?) CORBA IDL type as a TYPE-DEFINITION
  ((is-a
    :initarg :is-a
    :accessor type-definition-is-a
    :type type-definition))
  (:idl "typedef")
  (:metaclass idl-definition-kind))

#+:FELDSPAR
(defclass enum-definition (idl-definition)
  ()
  (:idl "enum")
  (:metaclass idl-definition-kind))

#+:FELDSPAR
(defclass sequence-type-definition (idl:typedef)
  ((element-type
    :iniarg :element-type
    :type idl:typedef
    :accessor sequence-element-type
    ))
  (:idl "sequence") ;; parse of: "typedef sequence"
  (:metaclass idl-definition-kind))


#+:FELDSPAR
(defmacro defmodule (name &rest args))


#+:FELDSPAR
(defmacro defidl-type (name &rest args)
  ;; "typedef" or "typdef sequence"
  )

#+:FELDSPAR
(defmacro defidl-enum (name &rest args))

#+:FELDSPAR
(defmacro defidl-exception (name &rest args))

;; - Naming Service

#+:FELDSPAR
(defmodule naming
    (:idl #:|CosNaming|)
  (:prefix "omg.org")
  (:version "1.3")
  ;; i.e has repository identifier "IDL:omg.org/CosNaming:1.3"
  )

#+FELDSPAR
(:in-definition naming)


#+:FELDSPAR
(defidl-type id-string
    (:idl "Istring")
  (:is-a idl:string)
  )


(defgeneric ncomponent-id (name-component))
(defgeneric (setf ncomponent-id) (new-id name-component))

(defgeneric nc-name (name-component))
(defgeneric (setf nc-name) (new-name name-component))


(defclass ncomponent () ;; NameComponent
  ((id
    :initarg :id
    :type
    #+:FELDSPAR id-string
    #-:FELDSPAR simple-string
    :accessor ncomponent-id
    
    #+:FELDSPAR :idl
    #+:FELDSPAR "id"
    )
   
   (kind
    :initarg :kind
    :type
    #+:FELDSPAR id-string
    #-:FELDSPAR simple-string
    :accessor ncomponent-kind
    
    #+:FELDSPAR :idl
    #+:FELDSPAR "kind"
    )
   )
   #+:FELDSPAR (:metaclass idl:struct)
   #+:FELDSPAR (:idl "NameComponent")
   )


#+:FELDSPAR
(defidl-type name
    (:idl "Name")
  (:is-a (idl:sequence ncomponent)))


#+FELDSPAR
(defidl-enum binding-type
    ((:name-object :idl "nobject")
     (:name-context :idl "ncontext")))


(defclass name-binding () ;; NameComponent
  ((name
    ;; should be of type "NameComponent" - cf. 1.3 spec
    ;; but was originally identified as being of type 'name'
    ;; and thus will be a name of length 1, indefinitely
    :initarg :name
    :type
    #+:FELDSPAR name
    #-:FELDSPAR simple-string
    :accessor name-binding-name
    
    #+:FELDSPAR :idl
    #+:FELDSPAR "binding_name"
    )
   
   (binding-type
    ;; used for binding-list, iterator-next-element, iterator-next-elements
    ;; not same as ../NameComponent/kind

    ;; TBD: Mapping from NAME-BINDING-TYPE to each of NAME-DIRECTORY, NAME-OBJECT
    ;; e.g (defun CLASS-FOR-BINDING-TYPE ...)
    ;;     (defun BINDING-TYPE-FOR-CLASS ...)
    
    :initarg :binding-type
    :type
    #+:FELDSPAR binding-type
    #-:FELDSPAR simple-string
    :accessor name-binding-type
    
    #+:FELDSPAR :idl
    #+:FELDSPAR "binding_type"
    )
   (object ;; IMPLEMENTATION SLOT
    :initarg :object
    :accessor name-binding-object
    :documentation
    "An object to which the name-binding resolves, when evaluated

See also: 
* `binding-type' [type]
* `context-resolve' [function]")
   
   )
   #+:FELDSPAR (:metaclass idl:struct)
   #+:FELDSPAR (:idl "Binding")
   )

(defclass name-directory (name-binding) ;; IMPLEMENTATIOM CLASS
  ()
  #+FELDSPAR (:default-initargs :binding-type :name-context ))

(defclass name-object (name-binding) ;; IMPLEMENTATIOM CLASS
  ()
  #+FELDSPAR (:default-initargs :binding-type :name-object ))


;; ...

#+:FELDSPAR
(defidl-type binding-list
    (:idl "BindingList")
  (:is-a (idl:sequence name-binding)))



(defclass naming-context ()
  ()
  #+:FELDSPAR (:metaclass idl:interface)
  #+:FELDSPAR (:idl "NamingContext")
  )

#+FELDSPAR
(in-definition naming-context)

#+FELDSPAR
(defidl-enum not-found-reason
    ((:missing-node :idl "missing_node")
     (:not-context :idl "not_context")
     (:not-object :idl "not_object"))
  )
  
#+FELDSPAR
(defidl-exception not-found ()
  ((why
    :initarg :why
    :type not-found-why
    :reader not-found-why
    :idl "why"
    )
   (rest-of-name
    :initarg :rest-of-name
    :type name
    :idl "rest_of_name"
    ))
  )


#+FELDSPAR
(defidl-exception cannot-proceed ()
  ;; FIXME: Extend his exception type in implementations ????
  ((context
    :initarg :context
    :type naming-context
    :idl "cxt")
   (rest-of-name
    :initarg :rest-of-name
    :type name
    :idl "rest_of_name")
   )
  )


#+FELDSPAR
(defidl-exception invalid-name ()
  )

#+FELDSPAR
(defidl-exception already-bound ()
  )


#+FELDSPAR
(defidl-exception not-empty ()
  )



(defgeneric context-bind (context name obj)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :void "bind"
        ((:in "Name" "n") (:in "Object" "obj"))
        :raises (not-found cannot-proceed invalid-name already-bound)))


(defgeneric context-rebind (context name obj)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl-lambda :void "rebind"
               ((:in "Name" "n") (:in "Object" "obj"))
               :raises (not-found cannot-proceed invalid-name)))


(defgeneric context-bind-context (context name context)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :void "bind_context"
        ((:in "Name" "n") (:in "NamingContext" "nc"))
        :raises (:not-found cannot-proceed invalid-name already-bound)))

(defgeneric context-rebind-context (context name context)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :void "rebind_context"
        ((:in "Name" "n") (:in "NamingCotext"))
        :raises (not-found cannot-proceed invalid-name)))


(defgeneric context-resolve (context name)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :object "resolve"
        ((:in "Name" "n"))
        :raises (not-found cannot-proceed invalid-name))
  #+TO-DO
  (:method ((context ...) (name ...))

   
    (let* ((the-first (the-sequence-first name))
           (the-rest (the-sequence-rest name))
           ;; find-the-name may throw-the-idl-exception not-found
           (the-inst (find-the-name the-fist context)))
      
      (unless (the-syntax-ok the-first)
        ;; syntax for the-first : name must be non-zero in length
        ;; and must conform to other implementation-specific
        ;; restrictions - e.g. constraints on network inteface names, etc
        (throw-the-idl-exception 'invalid-name ...
                                 ))

      (cond
        ((and the-inst the-rest)
         (context-resolve the-inst the-rest))
        (the-inst (values the-inxt))
        (t (throw-the-idl-exception 'not-found
                                    :instance context
                                    :why :missing-node
                                    :rest-of-name the-rest
                                    ))))))


(defgeneric context-unbind (context name)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :object "unbind"
        ((:in "Name" "n"))
        :raises (not-found cannot-proceed invalid-name)))

(defgeneric context-make-context (context)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl naming-context "new_context"))


(defgeneric context-bind-new-context (context name)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl naming-context "bind_new_context" (:in "Name" "n")
        :raises (not-found already-bound cannot-proceed invalid-name)
        ))

(defgeneric context-destroy (context)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :void "destroy"
        :raises (not-empty)))


(defgeneric context-list-elements (context)
  #+:FELDSPAR
  (:generic-function-class idl-interface-function)
  #+:FELDSPAR
  (:idl :void "list" ((:in (unsigned long) "how_many")
                      (:out binding-list "bl")
                      (:out binding-iterator "bi"))))


;; (defclass binding-iterator ()
;;   ()
;;   #+:FELDSPAR (:metaclass idl:interface)
;;   #+:FELDSPAR (:idl "BindingIterator")
;;   )
  

;; #+FELDSPAR
;; (:in-definition binding-iterator)

;; (defgeneric iterator-next-element (iterator binding-wrapper)
;;   #+:FELDSPAR
;;   (:generic-function-class idl-interface-function)
;;   #+:FELDSPAR
;;   (:idl :boolean "next_one" ((:out binding "b"))))

;; (defgeneric iterator-next-elements (iterator count list-wrapper)
;;   #+:FELDSPAR
;;   (:generic-function-class idl-interface-function)
;;   #+:FELDSPAR
;;   (:idl :boolean "next_n" ((:in (unsigned long) "how_many")
;;                            (:out binding-list "bl")))) 

;; (defgeneric iterator-destroy (iterator)
;;   #+:FELDSPAR
;;   (:generic-function-class idl-interface-function)
;;   #+:FELDSPAR
;;   (:idl :void "destroy"))

;; ;; Not addressed :
;; ;;  "IDL:omg.org/CosNaming:1.3/NamingContextExt" (TO DO)




;; ---- OLD NOTES



#+NIL-PROTOTYPE
(defun resolve-name (name)
  ;; Name kinds - an initial thesis
  ;;
  ;;  1. encoded name value
  ;;
  ;;     a value (typically encoded with a string or other sequence
  ;;     kind) representing a structured name, such that the the name
  ;;     may be parsed to an implementation-specific decoded form
  ;;
  ;;     e.g. URI, IOR, OID  value
  ;;
  ;;     applications:
  ;;       - URI, IOR, and OIDs may be written to or read from a
  ;;         stream, in certain network protocols
  ;;
  ;;  2. encoded object identifier
  ;;
  ;;     a value that may be evaluated in a specific name context,
  ;;     such as to retrieve an object not equivalent to the original
  ;;     value
  ;;
  ;;     e.g
  ;;
  ;;     - Common Lisp variable value, for a Common Lisp symbol
  ;;       evaluated onto the Common Lisp symbol value namespace
  ;;
  ;;     - Common Lisp function definition, for a Common Lisp symbol
  ;;       resolved onto the Common Lisp function namespace (excluding
  ;;       SETF functions or other CONS-named functions)
  ;;
  ;;     - Common Lisp class definition, for a Common Lispy symbo
  ;;       resolved onto the Common Lisp classes namespace
  ;;
  ;;     - CORBA object, broadly, for a CORBA IOR resolved to a CORBA
  ;;       object definition
  ;;
  ;;     - a STELLA qualified name - { <module name>, <symbol name> }
  ;;       with <module name> being a  sequence of module names
  ;;       delimited with "/" characters
  (multiple-value-bind (prefix str)
      (split-string-1 #\: name)
    (resolve-name-using-resolver str (find-name-resolver prefix))))


#+NIL-PROTOTYPE
(defun evaluate-name (name)
  ;; ...
  )


#|


Towards a methodology for name resolution in Common Lisp programs



e.g. in-naming-contet "/naming"

     asdf:<asdf-system-name>
     stella:sys:<stella-system-name>
     stella:mod:<stella-module-name> ;; tbd: memoization of system cardinal modules
     java:<java-class-name> ;; cf CL+J
     ldd:<shared-library-name> ;; cf. CFFI
     
informative fudge:

     uri:<uri-scheme-name>:<scheme-specific-part>
     urn:<urn-namespace-name>:<namespace-specific-part>
|#



