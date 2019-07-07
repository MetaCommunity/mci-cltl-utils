Storage Definitions - Lisp Tools Project, Thinkum Labs
======================================================

## Overview - Design of a Storage Definition System

**General Considerations**

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


* An _object_ may not, in all instances, be assumed to have been fully
  initialized from the perspective of a software program, even if fully
  initialized from the perspective of memory allocation for storage of
  the object, in the implementation.

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
      the resulting `CONS` object may be subsequently applied.

    * Usage Case: `MAKE-HASH-TABLE`, in which the generally _program-visible_
      storage of the object -- a _hash table_ -- has been **initialized
      for purpose of availability** to the program, before the _hash
      table_ is returned from the function. The _hash table_ may be
      subsequently **initialized for purpose of application**, by any
      portable or other approach, within the program.


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
      entail a static allocation, in the closure for the binding
      declared _dynamic extent_, such as  _vis a vis_ implementation for
      _stack space_ access. See also, SBCL; `alloca(3)` [BSD]

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

**Concerning Static Methods of Object Access**

Regardless of how an _object_ is allocated within a program's memory
space, there may be circumstances in which the _object_ can be accessed
in principally a static manner of access -- furthermore, in a manner
localized to the object itself, rather than _per se_ generalized to the
class of the object.


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

----

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
<!--  LocalWords:  Deallocated deallocated deallocation
 -->
