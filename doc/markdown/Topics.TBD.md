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
