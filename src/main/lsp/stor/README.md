Storage Definitions - Lisp Tools Project, Thinkum Labs
======================================================

## Overview - Design of a Storage Definition System in Common Lisp

The Storage Definition System [STOR] may endeavor to address a number of
complimentary concerns. These concerns are addressed, in an abbreviated
manner, below.

**General Considerations**

* Memory Allocation and Object Initialization in Common Lisp Programs
* Initialization of Storage Objects for Common Lisp Applications
* Static Allocation and Static Access in Common Lisp Programs
* Concurrent Access to Objects and Generalized Bindings
* Portability for Interactive and Non-Interactive Applications
* Class Specialization for Selection of Slot Definition Class, Given
  Multiple Available Slot Definition Protocols (**TBD**) - Procedures
  for Direct Slot Definitions and Effective Slot Definitions, onto MOP;
  Inheritance and shadowing for slot values, at initialization, across
  class superclass relations; Naming, Definition, Selection, and
  Inheritance for Slot Definition Protocols

----

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

#### Remarks - Concerning Relevance With Regards to External Objects

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


#### Remarks - Towards Some Formalism - Program and Application

This article, in a manner, develops a concept of a general dichotomy of
_program_ and _application_. In the sense as developed here, a _program_
may be defined in a manner such that: A _program_ interoperates with a
Common Lisp _implementation_, in any manner as may be generally
extensional to the Common Lisp programming language. A _program_ may
interoperate, furthermore, with any zero or more systems, such as may
be denoted _external to the implementation environment_, in any single
Common Lisp implementation.

In this sense, a _program_ may serve to provide support for an
_application_, while an _application_ would represent the principally
"User-Visible" features of a software system.

#### Remarks - Strongly Typed Implementations and Portable Programs

The Common Lisp programming language may be considered as supporting a
manner of general _weak typing_ in program definition. Regardless, the
Common Lisp language provides a comprehensive type system for
representation of scalar values, sequences, structured objects, and some
_control flow_ forms (e.g Restarts, Conditions, Tags, and Blocks) in
Common Lisp programs.

In a compiled Common Lisp program, regardless of any _weak typing_ in
the definition of the program's source form, the Common Lisp
implementation may be assumed to provide - in many regards - a _strongly
typed_ implementation of the compiled program.

In some implementations, such as CMUCL, the _strongly typed
implementation_ may be accompanied moreover with a comprehensive system
for _type inference_ and _call-form optimization_ in the compiled
program, however complimentary to any _bytecode encoding_ of the
compiled program, _vis a vis_ implementation "FASL Files".

**Aside: Concerning Reuse of Compiled "FASL Files" Forms of a Program**
Although the "FASL files" of a _compiled program_ may not be assumed to
be -- _per se_ -- _as portable as_ the program's source form, but --
assuming some limitations in an application of the Common Lisp language,
_per se_, and assuming any formal system for software accession, per
formal terms of software licensing restrictions, institutionally -- the
compiled "FASL files" form of a Common Lisp program _may be_ reusable in
_equivalent implementations_.

Towards interoperability with other programming systems, assuming a
general workflow for Common Lisp programming: Although such a workflow
may not seem to resemble -- in too many formal regards -- a general
workflow for programming with any other high-level language, yet some
general analogies may be developed towards applications of other
high-level languages, in juxtaposition to Common Lisp -- such as with
regards to:
- general _data flow_ for compilation, in producing any interpreted or
  compiled forms of a single _source form_
- strong typing in the compiled implementation of a software program,
  whether the type of a value is arrived at through type inference or
  user-declared type information, or both
- even as with regards to structures of _call forms_, in declarative
  _source forms_ and compiled _instruction forms_ of a program,
  singularly and in juxtaposition to any other programming languages.

Notwithstanding any _per se_ convenience of a _weakly typed_
programming style, in Common Lisp, but -- for some purposes of portable
application development with Common Lisp -- any number of _implementation
concerns_ -- such as may be reasonably entailed with definitions of an
object's _type_ -- may be considered to be quite program-visible. This
may be assumed to be _particularly so_,  in any programs interacting
with environments principally external to the programming environment
provided of any single Common Lisp implementation.

Insofar as that the Common Lisp programming language may be considered to
represent a generalized, vendor-neutral Lisp programming language, the
Common Lisp programming language may be applied -- similarly -- as in
definitions of portable programs, for any one or more Common Lisp
implementation. Insofar as that any single Common Lisp implementation
may provide, moreover, an interface for any implementation-specific
procedures that may be handled whatsoever _opaquely_, in the
implementation, any single Common Lisp application may endeavor to
support these implementation-specific features -- even insofar as with
regards to procedures for programmed reflection and inference onto the
implementation of the Common Lisp type system, if not expressly as with
regards to procedures for object allocation, initialization, storage,
reference counting, and deallocation -- in a portable manner.

**Ed. NB:** In a manner of phrasing, this may be the considered to
entail an approach of _programming without vendor-provided bubble-wrap._
As such, clearly, all licensing terms should be assumed to apply, as
with regards to indemnification and furthermore.

----

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

----

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

----

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

----

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
* Deallocated for Purposes of Availability or Application

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

----

## Additional Remarks

### `DEFCLASS` like `DEFSTRUCT` - Some General Considerations

**Ed. NB:** Juxtaposed to any discussions, per se, about _`DEFSTRUCT`
like `DEFCLASS`_, cf. proceedings of X3J13.

#### Remarks - On Reviewing `CL:DEFSTRUCT`

The original Common Lisp `DEFSTRUCT` provides, in a manner of a _macro
call form_, a methodology for definition of arbitrary _structured
objects_ in Common Lisp programs.

Consequent of the evaluation of a `CL:DEFSTRUCT` macro call form, the
definition of each such _structured object_ may be assumed to be
accompanied -- in effect -- with a number of complimentary _functional
definitions_, each closed onto a single definition of each such
_structured object_.

* **Constructor Definition** (Optional; Lambda List Syntax, in effect,
  constrained onto Structure Slot Names)
* **Accessor Definition** (For all slots, whether defined directly in a
  single structure class, or inherited from a single structure superclass)
* **Type Predicate Definition** (Juxtaposed to implementation-specific
  support for _type system_ reflection, in compiled programs)
* Intrinsically, _Slot Initform Definition_, in the definition of each
  _structure class_ itself.

**Ed. NB:** Corresponding with these functional definitions: FTYPE
declarations as - in some regards - effectively constant ...although an
FTYPE declaration may be modified at runtime, in Common Lisp,
theoretically for any function (however an FTYPE declaration for may be
ephemerally guarded for "Locked Packages," in some implementations
... "This language," by in large, lacking any comprehensive,
standardized access controls in programs.)

Furthermore, `DEFSTRUCT` provides a particular _feature_ for definition
of structure slots, such that may not be available elsewhere in the
primary language definition of Common Lisp. Specifically, there is the
`:READ-ONLY` initialization argument for structure slot definitions.


#### Synopsis - `DEFCLASS` like `DEFSTRUCT`

Concerning the functional forms -- for purposes of modularity and
generally, convenience in program definitions -- and considering the
singular `:READ-ONLY` initialization argument for structure slot
definitions, it may be useful to define a framework for providing
something functionally analogous to `DEFCLASS` but with a semantics
generally similar to `DEFSTRUCT`, in an implementation.

Insofar as that such an implementation may developed, in a manner,
portable onto Common Lisp Object System (CLOS) implementations per AMOP
-- _vis a vis_ PCL, e.g --the general `DEFCLASS` like `DEFSTRUCT`
implementation may need not be defined to be singularly dependent on the
characteristics of any one Common Lisp implementation.


### Generalized Binding State for Slot Definitions - Accessors

A naive model, with some description:

* `SLOT-VALUE-READABLE-P` (NB: May be removed subsequently, if
  equivalent functionality is available via `SLOT-BOUNDP` and
  `SLOT-BOUNDP-USING-CLASS`)

* `SLOT-VALUE-WRITABLE-P`
* `SLOT-VALUE-BINDABLE-P`
* `SLOT-VALUE-UNBINDABLE-P`

**Ed. NB:** Those may be implemented as to operate on the existing
binding state of an object and on the slot definition metaobject
representing the accessed slot, in the object's class -- rather than,
_per se_, defined as accessors onto the slot definition metaobject, in
itself.

For application onto slot definitions, as though trivially, a similar
set of accessors may be defined:

* `SLOT-DEFINED-READABLE-P` (NB: May be removed, subsequently)
* `SLOT-DEFINED-WRITABLE-P`
* `SLOT-DEFINED-BINDABLE-P`
* `SLOT-DEFINED-UNBINDABLE-P`

Towards a syntax, e.g for direct slot definitions:

> `:write (:bind :unbind :update)`

... in effect, towards a manner of "Equivalent Functionality" onto CLOS


> `:write (:bind :unbind)`

... in effect, not permitting change to any slot's binding, without
initial unbinding of the slot. **Ed. NB:** This may serve to illustrate
the meaning of the `:bind` `:unbind` specifiers, though it may not, in
itself, be very useful for applications.


> `:write (:bind)`

... in effect, not permitting any change to a bound slot's binding,
anywhere within this normal functional protocol. **Ed. NB:** See
precautions, denoted below.

> `:write (:unbind)`

... in effect. unusable, as it may be interpreted as not permitting
a slot to be initially bound.


Alternately, the implementation may support two slot definition
initialization arguments, for direct slot definitions:
* `:unbind-p`
* `:write-p`

**If assuming that any unbound slot can always be bound for purposes
of slot value storage,** the "Previous binding state" of a slot may need
not be accessed, in any such implementation. Furthermore, those two
initialization arguments may then be sufficient for specifying the
controls on a slot's binding state, in a manner generally analogous to
the `DEFSTRUCT` slot option, `:READ-ONLY`

Yet another alternate syntax might be considered, in which "Read" access
must be explicitly declared for a slot's value. Such a syntax might be
considered as representing a complete syntax onto possible slot-value
access procedures. However, considering the following, it may not
serve to provide a completely usable syntax.

> `:access (:read :bind :unbind)`
> `:access (:read :bind)`
> `:access (:read :unbind)`

... each of these being, in effect, interpreted in a manner similar to
each analogous expression in the `:write` syntax, described above.

> `:access (:bind)`

... in effect unusable, as it may be interpreted as not permitting
reading of a slot's bound value. Thus, this overall `:access` syntax may
be considered as providing a limitation.

**Ed. NB: The Non-Exclusive Nature of Portable Slot Access Controls Onto MOP**

In any regard, the definition of MOP's `STANDARD-INSTANCE-ACCESS`
function should be considered, for each implementation providing a
normal implementation-specific _location_ syntax, correlated with that
function as _per se_ usable for the function's second argument.  To this
effect, the common utility of PCL may bear some further consideration,
as towards portability in implementations having adopted PCL.

Considering the availability of the function, `STANDARD-INSTANCE-ACCESS`,
in implementations of MOP, it should not be assumed that a slot
definition protocol providing any extensional limitations for slot value
access -- in extension onto MOP, whatsoever portably -- may not be
providing, in the same, an altogether _exclusive limitation_ for slot
value access, within implementations.

Theoretically, an application may be created as as to subvert some
intermediate slot value access controls onto MOP, simply by calling that
function, directly, for any -- in effect -- access-controlled
slot. While this, in itself, may not require any odd register access or
mangling for bytecode object forms, it should be considered as a
potential exploit for any such informal slot access control methodology
onto MOP.

In a manner, it may be assumed, "The behaviors are undefined," if such
limitations would not be denoted in the systems documentation, and
furthermore addressed in applications.

Any discussion of formalized "Trust" for information systems would be
beyond the scope of this article.

Any discussion of formal methodologies for data system access controls,
within operating system environments and _userspace_ application
environments, similarly, would be beyond the scope of this article.

<!--  LocalWords:  Thinkum bitwise metaobject vis CDR CLtL DEFCONSTANT
 -->
<!--  LocalWords:  constantp APIs alloca se bytecode determinancy CLOS
 -->
<!--  LocalWords:  interprocess Deallocation syscalls ITERABLE ENUM PCL
 -->
<!--  LocalWords:  Accessors FUNCALL DEFSIGNATURE DEFIMPLEMENTATION TBD
 -->
<!--  LocalWords:  Deallocated deallocated deallocation STOR SBCL CMUCL
 -->
<!--  LocalWords:  unreferenced interoperation runtime interoperates
 -->
<!--  LocalWords:  interoperate FASL DEFCLASS DEFSTRUCT Accessor FTYPE
 -->
<!--  LocalWords:  superclass Initform modularity AMOP BINDABLE BOUNDP
 -->
<!--  LocalWords:  UNBINDABLE accessors MOP's portably userspace
 -->
