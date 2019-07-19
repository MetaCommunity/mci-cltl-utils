External Objects, OS Support - Thinkum Labs Lisp Tools Project
==============================================================

## Usage Cases - External Objects, OS Support

### Generalized Contexts

* POSIX/UNIX APIs and Implememtations
* libc APIs and Implememtations
* BSD, Linux, and Cygwin Environments (**Ed. NB:** Regarding Darwin as
  approxmimately a variant of BSD) (**Ed. NB, TBD:** Support for Open
  Solaris, subsq. IllumOS, Joyent OS dists) (**Ed. NB:** Android with
  Android libc,  **termux**  build/userspace environment, **termux**
  _app_ as a terminal emulator, optionally Android with Debian chroot
  environment, I/O with **termux** as a terminal emulator)

### Usage Cases, "Forked Lisp"

* Concurrent Backup and Storage for Lisp Image Data (**Ed NB:** in
  forked Lisp process)
* External Process Support (**Ed. NB:** See subsq.)
* Generalized Concurrency for Caller-Indpendent Tasks (**Ed. NB:**
  Portable _Task_ model, towards a superset of concerns addressed in the
  POSIX Threads and POSIX/UNIX Process APIs, addressed by POSIX/UNIX
  implementations in Arbitrary POSIX/UNIX host environments, incl. Cygwin)

### Usage Cases, External Process I/O

* I/O for SGML and XML Content with **nsgmls** ([SP][sp]), DSSSL Support
  for Editor Environments
* I/O for Entity Pathname/URI Resolution with SGML and XML Entity Catalogs
  ([libxml2][libxml2])
* Systems Analysis for Make-based Software Build Systems
  ([bmake][bmake], [GNU Make][gmake])
* Arbitrary Shell Commands in POSIX/UNIX Host Environments

### Usage Cases, POSIX/UNIX Pseudoterminal (PTY) Support

* PTY I/O for Arbitrary Shell Commands in POSIX/UNIX Host Environments
* Shell Emulation
* Systems Testing for REPL **editline**/**readline** Interoperability

### Usage Cases, POSIX/UNIX Filesystems (FS) Support

* Portable Descriptor-Based File I/O (**Ed. NB:** Implementation Streams)
* Temporary Files - Allocation, I/O, and Safe Removal (**Ed. NB,** See
  Also: External Process I/O)
* Support for Portable Management of POSIX/UNIX filesystem permissions,
  filesystem  flags, and file extended attributes

### Usage Cases, POSIX/UNIX Host Information/Support

* Portable Host Identity for Systems Management Tasks
* Knowledge Representation for Host Management
* Tooling, Interoperbility - CommonIDE

### Usage Cases, POSIX/UNIX Toolchain Information/Support

* Portable Toolchain, Component Identity for Systems Management Tasks
* Knowledge Representation for Toolchain Management
* Tooling, Interoperbility - CommonIDE
* **Ed. NB:** Syscall information library (BSD)
* **Ed. NB:** ELF (Non-Itanium) ABI support (Implementation-Specific
  Support for Typically OS-Specific Bytecode Library Support);
  `dlopen()` and related functions, support for host/other libc


## Design Documentation - API (Notes)

### OS Process Initialization, POSIX/UNIX Hosts - `fork()` without
  `exec()`

* TBD: IPC support for "Same Lisp" processes with subprocess
  initialization via `fork()`
    * TBD: Model for definition of data schema and protocol forms
        * ASN.1 (??)
        * CORBA GIOP/IIOP Protocols (??)
    * TBD: Implementation support for data schema and protocol
      definitions
        * NB: POSIX RT mqueue API
            * OS Support - BSD, Linux (TBD: Alternatives in other OSes,
              e.g Microsoft PC OSes)
            * May not require special initialization for the `fork()`
              usage case
            * API definition may support mqueue identification, pursuant
              to the call to `fork()`
        * NB: ASN.1 tools for C program systems; general implementation
          support for FFI with C languge comonents
        * NB: ORBit support for C languge components
            * IPC over TCP/IP and UNIX address family sockets
                * NB filesystem permissions for UNIX sockets
            * TBD: Object system integration and protocol support for
              GIOP/IIOP CDR encoding with the GNOME ORBit API, independent
              of any complete ORB environment
            * NB: Reusable CORBA implementation (GNOME)

### OS Process Initialization, POSIX/UNIX Hosts - `fork()` and `exec()`

* NB: Concerns pursuant to/after process `fork()` - _Resource
  Management_ in the _Forked Process_
    * Generalized Concerns - Close I/O streams in Forked Process
      (prevent ambiguous PTY I/O)
    * Implementation-Specific Concerns - Provide Generalized "Close On
      Exec" Support, in Implementation
    * Implementation-Specific Concerns - Provide Optional "External
      Object Handling" in Forked Process (e.g object deinitialization
      and deallocation, resource release, and optional bytecode library
      unloading)

* NB: Concerns pursuant to/after process `fork()` - Process
  Characteristics (POSIX/UNIX host environments)
    * Process Environment
    * Process Group and (when applicable) Controlling Terminal (NB: stdio)
    * Real and Effective Principal ID (UID, GID) (NB: Differing only
      for processes with "switch user" permissions)
    * Process I/O Descriptors (stdio, other)
    * Process resource limits (NB: Host-specified resource limits i.e
      "Hard Limits," and user-specified i.e "Soft Limits")

* NB: Concerns pursuant to implementation of the fork/exec call form:
    * Provide string-to-temporary-file management within the forking
      process (temporary file creation, optional post-process cleanup)
      and the forked process (open file and `dup()` for standard/other
      descriptor I/O; close and delete file before normal exit)

    * Provide support for monitoring the subprocess runtime state - UNIX
      signals (i.e `SIGCHILD`, see also **siginfo(3)** noting `CLD_...`
      code values)

    * TBD: Debugger-over-IPC support (Protocol, inoke-debugger
      intgration, and REPL UI) for interactive management of exceptional
      situations in the forked process (NB, Usage Case: Interactive
      programming/debugging for forked processes, before/without exec)



## References

* [KR1988] Kernighan, Brian W., and Dennis M. Ritchie. The C Programming
  Language. 2nd ed. Englewood Cliffs, N.J: Prentice Hall, 1988.
* [SUSv4] [Single UNIX® Specifiation, Version 4][susv4], 2018 Edition. X/Open
* BSD Manual Pages
* Linux Manual Pages - GNU Compiler Collection (GCC), Linux Kernel,
  Linux Distribution Userspace
* Other Supporting Documentation

[sp]: http://www.jclark.com/sp/
[libxml2]: http://xmlsoft.org/
[bmake]: http://www.crufty.net/help/sjg/bmake.html
[gmake]: http://www.gnu.org/software/make/
[susv4]: https://publications.opengroup.org/t101
