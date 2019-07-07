Storage Definitions - Lisp Tools Project, Thinkum Labs
======================================================

## Overview - Design of a Storage Definition System in Common Lisp

The Storage Definition System [STOR] may endeavor to address a number of
complimentary concerns. These concerns are addressed, in an abbreviated
manner, below.

**General Considerations**

* Memory Allocation and Object Initialization in Common Lisp Programs
* Initialization of Storage Objects for Common Lisp Applications
* Static Allocation and Access in Common Lisp Programs
* Concurrent Access to Objects and Generalized Bindings
* Portability for Interactive and Non-Interactive Applications



### Memory Allocation and Object Initialization in Common Lisp Programs

* For representation of _internal objects_ in a Lisp programming
  environment, initial _allocation_ and subsequent _initialization_ of
  memory resources - those directly used by the object - would typically
  be handled in a manner transparent to the application.

    * Any details of the _allocation size_ and bitwise _alignment_ for
      the _internal object_ may be assumed to be handled -- in a manner
      principally local to the Lisp implementation -- during the
      _allocation_ and _initialization_ of the memory resource for the
      _internal object_.

    * The _internal type_ of the _object_ -- in a manner similarly local
      to the Lisp implementation, moreover principally _constant_ to the
      object itself -- may be assumed to be stored for the object, also,
      during _allocation_ and _initialization_ of the memory resource
      representing the object.

#### Remarks - Relevance With Regards to External Objects

* It may be assumed --  if only for purpose of convenience -- that any
  implementation-specific procedures for object allocation and object
  initialization -- as for purposes of object access within Common Lisp
  programs -- may be handled in a manner by-in-large opaque to the
  program.

* For any object initialized in a manner principally external to the
  Common Lisp implementation environment -- such as in any applications
  of external bytecode libraries, onto any foreign functions interface,
  such as may be available in a Common Lisp implementation -- the
  concerns of object allocation and object initialization may need to be
  addressed directly by the program.

* Furthermore, while it may be assumed that any Common Lisp
  implementation may provide a system for determining when a "Lisp
  Object," broadly, is unreferenced and may be deallocated within
  garbage collection, and while this system may typically operate in a
  manner generally opaque to Common Lisp applications,  but such
  concerns may may need to be addressed directly by the program, for any
  program operating onto external bytecode libraries within a software
  host environment.

* **Ed. NB:** In a regard, the _convenience_ of such generally opaque
  procedures for memory allocation, object initialization, object
  reference counting and garbage collection may not seem _as convenient_,
  when those procedures must be addressed for interoperation with other
  programming systems. In any regard, such procedures must be addressed
  in an implementation-specific manner.

* This section of the article endeavors to address concerns with regards
  to _allocation and object initialization for purposes of program
  access_ onto arbitrary _objects_. This may not, in itself, serve to
  address all concerns with regards to object initialization for
  _application access_, whether for objects defined as principally
  _within_ or _external to_ a Common Lisp implementation environment.


**Ed. NB: Towards Some Formalism - Program and Application**

This article, in a manner, develops a concept of a general dichotomy of
_program_ and _application_. In the sense as developed here, a _program_
may be defined in a manner such that: A _program_ interoperates with a
Common Lisp _implementation_, in any manner as may be generally
extensional to the Common Lisp programming language. A _program_ may
interoperate, furthermore, with any zero or more systems, such as as may
be denoted _external to the implementation environment_.

In this sense, a _program_ may serve to provide support for an
_application_, while an _application_ would represent the principally
"User-Visible" features of a software system.


### Initialization of Storage Objects for Common Lisp Applications

* An _object_ may not, in all instances, be assumed to have been fully
  initialized from the perspective of a software application, even if
  fully initialized from the perspective of memory allocation for
  storage, in an implementation.

    * Usage Case: `MAKE-ARRAY`, in which the `:INITIAL-CONTENTS`
      keyword parameter provides a manner of access towards
      _initialization for purpose of application_, semantically near
      to -- in a manner, opaquely simultaneous to -- _initialization for
      purpose of availability_ to the calling program.

    * Usage Case: `ALLOCATE-INSTANCE`, during object initialization;
      Note additional/specialized usage cases in MOP implementations,
      such as for _type system_ integration during metaobject
      initialization _vis a vis_ `ENSURE-CLASS`

    * Usage Case: `CONS`, in which the `CAR` and the `CDR` of each _cons
      cell_ may be initialized directly in the call to `CONS` -- however
      the resulting `CONS` object may subsequently be applied, whether
      in effect applied as a generally read-only storage or
      destructively modified in any subsequent procedure.

    * Usage Case: `MAKE-HASH-TABLE`, in which the generally _program-visible_
      storage of the object -- a _hash table_ -- has been **initialized
      for purpose of availability** to the program, before the _hash
      table_ is returned from the function. The _hash table_ may be
      subsequently **initialized for purpose of application**, by any
      portable or other approach, within the program.

#### Remarks - Towards Application Support

* An application may provide -- from the perspective of an application
  user -- any principally opaque method for initializing any storage
  objects, such that may be accessed by the application.

* For purposes of application development onto arbitrary data storage
  methodologies, such that may be assumed to entail at least a _local
  storage_ if not furthermore a complimentary _external storage_ for
  program objects, an application system may endeavor to provide some
  general development support.


### Static Allocation and Access in Common Lisp Programs

* Common Lisp -- in the pedagogic CLtL2/ANSI CL -- provides a limited
  support for portable _static allocation_ of objects.

    * Usage Case: `DEFCONSTANT` albeit such that the constant variable's
      binding may be altered, as within an interactive programming
      environment. (Note that this form operates on variable bindings in
      the null lexical environment, insofar as entailing a reference to
      an object under a symbolic variable name -- for a variable, as
      such, defined with `DEFCONSTANT`)

    * Usage Case: `MAKE-LOAD-FORM` when specified as generally
      "constantp," and when supported by the implementation

    * A Generally "Special Allocation" may be used by an implementation,
      for some objects declared `DYNAMIC-EXTENT`. This may or may not
      entail a static, ephemeral allocation, in the closure for the
      binding declared _dynamic extent_, such as  _vis a vis_
      implementation _stack space_ access. See also, SBCL; `alloca(3)`[BSD]

    * An implementation may, at times, detect and emit an _error
      condition_ when constant data is accessed with a "write"
      operation -- i.e in a which a program endeavors to produce a
      destructive modification to a constant object. Such a condition
      may be encountered as when compiling program source code
      interpreted from a Lisp source file.

    * Concerning the operating system environment of a Lisp
      implementation, this article assumes that such _static storage_
      generally may be entailed with storage of an object within a
      _stack space_, in a manner generally implementation-defined if not
      _per se_ limited to an OS process.

    * Concerning analogies onto other programming systems, such as C,
      some kinds of -- e.g any _globally scoped_ -- _static object_
      may generally be allocated in a constant manner, within a compiler
      environment, and may be  provided with specific storage in
      bytecode object files. Other _static storage_ may be available,
      within any platform-specific limitations, for call-specific forms
      produced by a C compiler.

#### Remarks - Concerning Static Methods of Object Access

Regardless of how an _object_ is allocated within a program's memory
space, there may be circumstances in which the _object_ can be accessed
in principally a static manner of access -- furthermore, in a manner
localized to the object itself, rather than _per se_ generalized to the
class of the object.

#### Remarks - Towards Analogy for Other Software Programming Systems

One might observe some general analogies towards software programming
systems other than those implemented in Common Lisp, such as with
regards to:

* Storage and access for static data, in bytecode files and in procedural
  runtime environments

* Storage and reference for generally symbolic binding information

* Storage and access for objects representing functional operations


### Concurrent Access to Objects and Generalized Bindings

* **Concerning Concurrency**

    * The "Object of Concern," in a manner: Any object that may be
      modified within _more than one_ of a concurrent, _generalized
      task_ procedure - towards a generalized concept: Guarded (Mutually
      Exclusive/Modal) Access, for purpose of ensuring determinancy in
      generalized "Access Scheduling" and subsequent application
      procedures.

        * This may be construed as to also indicate: Any generalized
          variable, generalized functional form, or generalized slot
          whose binding may be destructively modified (e.g from a state
          "Unbound" to a state "Bound," or conversely "Bound" to
          "Unbound," or "Bound" to "New binding") ... in a generally
          similar manner.

        * May be of some academic interest, moreover of a practical
          interest with regards to portability for implementations
          providing only a "single threaded support," within any single
          operating system process, on any single hardware
          architecture. (Note that some concerns of "Single threaded
          support" may be, in some regards, furthermore alleviated with
          an application of any suitable interprocess communication
          system, on operating systems supporting multiple concurrent
          processes.)

        * [...]

#### Remarks - Concurrent Access for Objects and Bindings

**Ed. NB:** Clearly, this topic needs further development.


### Portability for Interactive and Non-Interactive Applications

Common Lisp, in a manner as may be considered unique among other systems
programming languages, provides a broad manner of support for usage
cases in _Interactive Programming_.

Insofar as for applications in non-interactive environments, some
assumptions may be made such that may serve in a general sense of
limitations onto the Common Lisp programming language -- moreover, such
as may be realized onto any one or more implementation of the Common
Lisp programming language. Some general considerations, as such:

* Initialization and subsequent access to objects and bindings -- in a
  general regard, may be restricted to "Read-Only" Access for some
  generally initialized objects and bindings.

* Class finalization -- in a general regard, should be assumed as
  _possible_ if not, in an initial state, _completed_ for every defined
  class.

* Method availability, onto CLOS generic functions -- in a general
  regard, _static_ within the limitations of this usage case.

* [...]

----

## Additional Concerns

* External Objects - Allocation, Initialization, Internal Access
  (Static/Ephemeral) and subsequent (when applicable) Deallocation

* Interoperability within Desktop Application Environments - Allocation,
  Initialization, Internal Access, and Deallocation for Generalized
  Service Pointers, onto Generalized Service Descriptors (NB:
  Non-exclusive with regards to component vendors)

* Interoperability onto a Service-Oriented Model of the Operating
  System Kernel - for arbitrary Operating System Kinds (e.g Generally
  UNIX or Generally "Other") and Operating System Implementations --
  Kernel Service Descriptors and UNIX syscalls (e.g)

----

## Relevance in API

### Prototype Systems

* `ITERABLE-CLASS`, `ITERABLE-OBJECT`; `MAP-ITERABLE` Accessors
    * `ltp-main:src/main/lsp/iter/iter.lisp`

* `ENUM`, `DYNAMIC-ENUM`, `STATIC-ENUM`; `ENUM-MEMBERS` Accessors
    * `ltp-main:src/main/lsp/mop/enum/enum.lisp`

#### Support Systems

* `WITH-WRITE-GUARD`; `GUARDED-FUNCALL`
    * `ltp-main:src/main/lsp/mt/guarding.lisp`

* `DEFSIGNATURE` and `DEFIMPLEMENTATION`
    * `ltp-main:src/main/lsp/mt/defportable.lisp`

* Base Class Definition - Design Documentation
    * `ltp-main:src/main/lsp/base-class/README.md`


### Towards a Diagrammatic Representation of APIs

Generalized Descriptions of _Storage State_ in Object Storage:

* Declared for Purpose of Programmed Analysis
* Initialized for Purpose of Availability
* Initialized for Purpose of Application
* Deallocated

These terms are approximately described in the previous text of this
article.

### Concerning Implementation

* For any object that may be defined, at one state, as _finalized_ for
  purpose of application, a question: As previous to any assertion that
  the object is in a _finalized_ state, as such, How is the object
  initialized, for purpose of application? More specifically, How are
  any data values provided for the object's initialization?

* For any object that may be defined as _ephemeral_ for purpose of
  application, a question: If the object makes reference to other
  objects, should any of those references be manually deallocated within
  any task of the defining program, and how so, moreover without -- in
  any side effect -- recursively unbinding every generally bound state,
  within the application environment? Furthermore, when may it be
  inferred that the object will no longer be referenced in any location
  of the object's ephemeral application? **Note that a systems
  programmer may provide additional information, to this effect,**
  pursuant of any support for declarations with regards to object
  deallocation. Ideally, any "Deallocation Declarations" may be provided
  as when a program is initially defined. Albeit, this may serve to
  establish some more of any effective limitation for systems
  programming -- in Common Lisp as a programming language finding, in a
  general regard, an interactive application. Perhaps, it may be
  approached in a manner that will not preclude interactive programming,
  however, within any software system defined under any discrete limits
  as such. **Ed. NB:** This article does not assume as though the
  systems programmer should not be provided with any manner of control
  for object deallocation, however any corresponding procedures might be
  assumed to be handled by an implementation's "Garbage Collector."

* For any object that may be defined as _non-ephemeral_ and
  _non-static_ for general purposes of object storage, How may that
  object be accessed in a most effective manner? Regardless of the
  _non-static_ nature of the object, may the object be accessed with a
  static function operating directly on any internal storage utilized by
  the object, with that function _per se_ defined as exclusive to the
  individual object? Any such instance-specific function, of course, may
  be initialized with a procedure generalized to the class of the
  object.

<!--  LocalWords:  Thinkum bitwise metaobject vis CDR CLtL DEFCONSTANT
 -->
<!--  LocalWords:  constantp APIs alloca se bytecode determinancy CLOS
 -->
<!--  LocalWords:  interprocess Deallocation syscalls ITERABLE ENUM
 -->
<!--  LocalWords:  Accessors FUNCALL DEFSIGNATURE DEFIMPLEMENTATION
 -->
<!--  LocalWords:  Deallocated deallocated deallocation STOR SBCL
 -->
<!--  LocalWords:  unreferenced interoperation runtime interoperates
 -->
<!--  LocalWords:  interoperate
 -->
