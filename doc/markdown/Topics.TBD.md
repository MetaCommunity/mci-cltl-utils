Topics - LTP Common
===================

## Topic: Global Locking During `*FEATURES*` Eval

**Context:** Multi-Threaded Implememtations

**See Also**
- `LTP/COMMON:FEATUREP`

**Remarks**

- `*FEATURES*` may be destructively modified within multi-threaded
  implememtations, as a side-effect of concurrent evaluation of
  arbitrary source forms

- Some multi-threaded implementations may provide a generic, global
  lock, such as may be held exclusively during any generalized operation
  that may modify the set of global definitions -- i.e definitions in
  the pedagogic _null lexical environment_. When provided with a known
  interface to this global lock, operations onto `*FEATURES*` may
  similarly hold this global lock, for the duration of any evaluation or
  modification onto `*FEATURES*`

- When no such interface is known to be available to applications, a
  generic application-specific lock may be used, in a manner semantically
  similar to any generic global lock as may be provided by any single
  implementation.

- Insofar as no explicit specification has been adopted, in any
  well-known standard, for concurrent access onto `*FEATURES*`, it should
  be assumed that any localized methodology for ensuring mutually
  exclusive access to `*FEATURES*` may not be utilized by all
  applications as may access the value of `*FEATURES*`. Assuming that
  a system may be able to ensure, at least, that any source forms not
  ensuring mutually exclusive access to `*FEATURES*` will not be
  evaluated simultaneous to any source forms requiring mutually
  exclusive access to `*FEATURES*`, any possible conflicts of scheduling
  for access to the value of `*FEATURES*` may be, in theory, minimized.

----

## Topic: `CONDITION` object is not ever typep `SLOT-OBJECT` in some SBCL

With a source form evaluated under SBCL version 1.4.16, an inconsistency
may be observed in the implementation -- principally, as with regards to
the class precedence list of the class `CONDITION`, under `TYPEP` when
evaluated for `CONDITION` objects.

### Known Affected Calls - `TYPEP` onto `CONDITION` Class Prototype

An illustration of this incnosistency in the implementation type system
follows. For purpose of brevity, this illutration uses forms accessible
in the `LTP/COMMON/MOP` namespace, furthermore definiing a global
variable in that namespace, such that may be applied in verification of
the inconsistency.

    (in-package #:ltp/common/mop)

    (defparameter *common-impl-type*
      (class-name
       (least-common-superclass
        'condition
        (least-common-superclass 'standard-object
                                 'structure-object))))

    *COMMON-IMPL-TYPE*
    ;; => SB-PCL::SLOT-OBJECT

    (typep (class-prototype (find-class 'standard-class))
           *common-impl-type*)
    ;; => T

    (typep (class-prototype (find-class 'structure-class))
           *common-impl-type*)
    ;; => T

    (typep (class-prototype (find-class 'condition))
           *common-impl-type*)
    ;; => NIL


The return value of the last form, in that example, is inconsistent onto
the class precedence list of the class, `CONDITION`.


### Known Affected Calls - `TYPEP` onto Initialized `CONDITION`

The previous might seem, at first, that it may be a side-effect from the
usage of the `CLASS-PROTOTYPE` of the `CONDITION` class -- the _class
prototype_ object not representing an initialized `CONDITION` object.

This inconsistency may be verified, furthermore, using an initialized
`CONDITION` object, such as in the following.

    (let (%typep)
      (handler-bind ((condition (lambda (c)
                                  (setq %typep
                                        (cons (typep c *common-impl-type*)
                                              c)))))
        (signal 'condition))
      (values %typep))
    ;; => (NIL . #<CONDITION {1004EBBE93}>)


The test orchestrated via `HANDLER-BIND` illustrates a similar
inconsistency -- namely, in the call to `TYPEP` with a `CONDITION`
object and the class `SB-PCL::SLOT-OBJECT`, in the implementation.


### Known Affected Calls - `SUBTYPEP` for the class `CONDITION`

This inconsistency in the implementation type system may be verified,
futhermore, using `SUBTYPEP` in the implementation.

    (subtypep 'condition 'sb-pcl::slot-object)
    ;; => NIL, T


### Summary

In this example, the type `CONDITION` is illustrated as being
inconsistent for `SUBTYPEP` with the class `SB-PCL::SLOT-OBJECT`.

Similarly, an inconsistency is illustrated for `CONDITION` objects,
as inconsistent to the class precedence list of the class, `CONDITION`.

This inconsistency may be summarized, as so: In an affected
implementation, `CONDITION` objects will not be evaluated as `TYPEP`
onto the class `SB-PCL::SLOT-OBJECT`, although that class appears in the
class precedence list of the class, `CONDITION`.

This insconsistency in the type system may affect applications relying
on some type relations of the class, `CONDITION`, when evaluated in an
affected implementation.


### Estimate in QA

Considering the nature of the implementation of CLOS, in SBCL, and the
nature of the compilation environment for building SBCL, this summary
presents an assumption: That it may be non-trivial to address this
inconsistency in the implementation type system.

### Known Workarounds

In the LTP Common MOP system, this inconsistency has been addressed --
in a manner of an effective workaround -- by removing the class
`CONDITION` from the form computing the definition of the portable
`LTP/COMMON/MOP:INSTANCE` type.

This change does not result in any syntactic difference for the
definition of the type, `LTP/COMMON/MOP:INSTANCE`, in that
implementation. However, it is believed to be more structually
consistent in that it does not involve any operation onto the class
precedence list of `CONDITION`, for any implementation.
