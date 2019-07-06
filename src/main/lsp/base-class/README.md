Base Class Definition - Lisp Tools Project, Thinkum Labs
========================================================


## See Also

* Towards application support, _Storage Definitions_
    * `ltp-main:src/main/lsp/stor/README.md`

## Base Class Implementation Support - Design Documentation

**General Considerations**

* Class Specialization, in Roles for _Slot Definition Initialization_
  and Subsequent _Slot Value Access_ Forms - per MOP as in extension to
  CLOS, furthermore considering the generally polymorphic nature of
  methods onto CLOS (for purpose of discussion, optionally not
  regarding `EQL` specialization for Methods in CLOS and MOP)

* Class Specialization, Class Metaobject Definition and Initialization
  in MOP, and MOP `VALIDATE-SUPERCLASS`


**Other Considerations**

* Class Specialization, in Roles for Generic Function and Method
  Initialization - per MOP as in extension to CLOS

* TBD: _Template-Like_ Semantics and Syntax for Class Specialization
  onto CLOS

* NB: _Type Description_ as a Principal Role of a Class Definition -
  Concerning _Type Descriptions_ in Structure and Implementation of
  _Class Definitions_, and _Type Descriptions_ in Definition and
  Implementation of _Polymorphic Functional Forms_; Juxtapose to _Value
  Encapsulation_ as a Principal Role of an _Object_; Note a Perhaps
  Generally _Frame-Like_ Semantics/Syntax of Class Definitions and Slot
  Definitions in CLOS

* TBD: Portable Definition of _Signature_ and _Implementation_
  Specifications for Class Definitions and Polymorphic Functions, onto
  CLOS; Model-Oriented Representations, Diagram Semantics, and Visual
  Representations for Class Signatures and Other Effective Metaobject
  Signatures, in Systems Documentation and Systems Design -- for purpose
  of discussion, not limiting the definition of _metaobject_ singularly
  to MOP, neither limiting the modeling domain singularly to the
  Semantics of of "Class-Owned" Specifications for Polymorphic
  Functions.


## Slot Definitions for Base Class Implementations - Design Documentation

**Features**
* Access Specifiers for Slot Definitions
* Strongly Typed Accessors for Slot Definitions
* _Support for_ Managed Concurrency for Slot Value Access

### Synopsis - Access Specifiers for Slot Definitions

**API Design (Notes)**

Access specifier symbols:

* `:read` - Read slot value binding (NB: In most cases, needed)

* `:bind` - Bind slot value, when unbound (NB: In most cases, this or
  `:write` access would be needed, for each slot definition)

* `:write` - Change slot value binding, regardless of the the slot's
   initial binding state (may be represented as a generalization of
   `:bind` access, without regards to immediate slot value binding
   state)

* `:unbind` - Unbind slot value, when bound

* `:bind-once` - Mutually exclusive to `:bind`, `:unbind` and
  `:write`. Bind slot value, when unbound; do not permit unbinding


Initialization argument for direct slot definitions: `:access` - syntax,
an _access specifier symbol_ or a list of _access specifier symbols_.

TBD: Application for class-allocated slots

TBD: Inheritance and overriding for slot-value access specifiers, within
effective slot definitions of effective subclasses

Generalized Accessors
- `SLOT-VALUE-READABLE-P`
- `SLOT-VALUE-WRITABLE-P`
- `SLOT-VALUE-BINDABLE-P`
- `SLOT-VALUE-UNBINDABLE-P`

**General Considerations**

* Does not comprise, per se, a complete _slot value access management_
  protocol, such as _vis a vis_ interfaces onto _system user_ permission
  environments (UNIX file systems, TCP/IP Network systems, other)

* Does not comprise, per se, a complete _software permissions
  management_ protocol, such as _vis a vis_ formalized systems of
  _trust_, _accession_ and _verification_ for software components.

* Does not comprise a _data permissions management_ protocol, such as
  for any formalized definition _trust_, _accession_ and _verification_
  for arbitrary systems of _data services_


### Synopsis - Strongly Typed Accessors for Slot Definitions

**API Design (Notes)**

* Approximately, a feature of a "Defclass like Defstruct"
  implementation - such that may be accompanied with support for
  defstruct-like constructor definition -- with corresponding `FTYPE`
  proclamations -- during class initialization

* TBD: Protocol for accessor update, when class definitions are
  dynamically updated - note limitations as may be pursuant of a
  requirement for consistent `FTYPE` declarations within compiled code

* TBD: Standard Instance Access (Class Layout-Dependent) for accessors
  onto MOP - Usage Cases: Check slot binding state; Read slot value;
  Bind slot value, Unbind slot value

* TBD: Integration with access specifiers for slot binding state

### Synopsis - Managed Concurrency for Slot Value Access

**API Design (Notes)**

* Must be portable -- in essence, should be transparent -- for
  applications in single-threaded implementations. In which
  implementations, note furthermore: OS Multi-Processing Models;
  Conventions for Interprocess Communication in OS Multi-Processing
  Models (Single-Host Environments and Network Systems Environments)

* Note that this _feature set_ may be implemented in an extension onto
  `BASE-CLASS`, rather than as a feature of the `BASE-CLASS` definition,
  in itself. Note, furthermore, the previous remark with regards to
  _portability_ in implementation.


**General Considerations**

* Modal (read/write mutex) Locking, when supported by the
  implementation - such as may be provided via a system providing a
  generalization onto implementation-specific features - **NB** _some
  prototyping_, e.g `GUARDED-FUNCALL` for location-independent
  mutual/modal exclusion in a critical section of a program,
  encapsulated in a  functional form, a prototype implementation
  currently developed in `ltp-main:src/main/lsp/mt/guarding.lisp`,
  presently paused for further refinement of the design and
  implementation of `DEFSIGNATURE` and `DEFIMPLEMENTATION` forms in the
  `ltp-main` source repository.

* Global Mutex Pooling - cf. `guarding.lisp` - onto a generalized
  prototype for _object pool_ definition, the latter in a definition
  developed perhaps without immediate integration for any generalized
  operations for _localized_ or _process-wide_ object locking, though
  ideally extensible under a portable methodology for such operations,
  in concurrent programs.

* **For implementations supporting modal locking,** acquire _read_ lock
  when reading slot binding state or slot value (must not prevent other
  concurrent read; must prevent concurrent change of the slot value
  binding state or slot value, while the read lock is held)

* **For implementations supporting modal locking,** acquire _write_ lock
  when changing slot binding state (bound -> unbound) or setting a new
  slot value (must prevents all concurrent read or slot value binding
  change, within the duration of any single "Locked" slot value
  transaction)

* **For implementations not supporting modal locking,** use mutually
  exclusive access during read, bind, or unbind operations for slot
  value bindings; impact of the _absence of modal locking support_, for
  applications, might assumed to be - in effect - negligible for
  concurrent changes to slot value binding state, though limiting for
  concurrent slot value binding "read"

* **For all implementations providing a "Well Known" mutex-like form**,
  the effective, temporary "Locked State" of a slot's binding within an
  object must be indicated in an unambiguous regard, as representative
  of a slot value's binding state; this may be considered for
  application onto both class-allocated and normative instance-allocated
  class slots.

* **Allocation and access for slot locking storage** may be approached
  in a manner per each class definition, extensional to a known set of
  metaclasses providing any support for differing methodologies for such
  allocation and access. Usage cases and implementation methodologies
  may differ e.g as per the number of the slots (e.g generally "One or
  "More than One") such that would be defined for locking under
  mutual/modal exclusion onto individual objects -- in any
  single class definition however applying any provided metaclass
  definition. "This system," as in extension to `BASE-CLASS`, may
  endeavor to provide more than one such metaclass definition.

* Implementation of this _feature set_, itself, _may be approached_ in a
  manner perhaps reminiscent of _slot layout_ information in PCL
  implementations of MOP -- in a methodology for class definitions
  entailing more than one "Synchronized Slot Value". Assuming that each
  effective slot definition within such a supporting class is defined
  with a unique index onto an instance layout vector, a secondary "Mutex
  Vector" may be developed for each such instance. A "Global Lock" may
  be acquired (whether in read, write, or all-mutually-exclusive access
  protocols) on a specific well-known storage location (in global
  storage, or rather, specific to the defining class of the instance),
  pursuant to checking the state of any slot within any instance's
  "Mutex vector," as such. If any "initialized mutex" -- as may be
  initialized from a global or localized "Mutex Pool" -- is located for
  that slot, a lock may be requested on that instance/slot-specific
  mutex, and the global (or class-local?) mutex then released. If no
  mutex is found as in effect "Bound" for the slot value's access, a new
  mutex may be initialized and stored in the slot's location within the
  layout mutex vector, and the global/class-localized "Mutex check" lock
  then released. Across either usage sub-case, the "instance slot lock"
  would be held -- in a "Lock hold" mode appropriate for both (A) the
  access to the requested operation on slot-value binding, and (B) to
  the support for POSIX-like concurrent access forms, in the
  implementation -- for the duration of the corresponding operation on
  slot-value binding, and must be released on normal or abnormal exit
  from that operation. Note that the lock-release procedure for the
  instance/slot lock may entail acquiring the global or class-local
  lock, in a manner analogous to the initial lock-acquisition
  procedure. However, in releasing the lock, any unused lock may be
  subsequently returned to a global "Mutex Pool," for later reuse in
  other concurrent slot binding access operations.

* This system may utilize one or more _globally scoped_ (as in the null
  lexical environment) and/or _class scoped_ (as per an instance's
  defining class) synchronization structures, pursuant towards _mutex
  pooling_ and generalized implementation for mutually exclusive/modal
  access onto _guarded_ program sections, including guarded program
  sections as may provided access to specific storage values within a
  program.

## General Considerations - System Design Documentation

TBD - See also: TeX; TEI P5; DocBook 5; GNOME Devhelp and gtk-doc tools

<!--  LocalWords:  Thinkum CLOS EQL Metaobject SUPERCLASS TBD Accessors
 -->
<!--  LocalWords:  subclasses BINDABLE UNBINDABLE se vis TCP Defclass
 -->
<!--  LocalWords:  Defstruct defstruct FTYPE accessor accessors mutex
 -->
<!--  LocalWords:  funcall defsignature defimplementation ltp PCL POSIX
 -->
<!--  LocalWords:  Interprocess metaclasses metaclass
 -->
