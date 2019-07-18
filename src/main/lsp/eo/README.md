External Objects - Thinkum Labs Lisp Tools Project
==================================================

## Usage Cases - Thinkum Labs

### Usage Cases, "Forked Lisp"

* Concurrent Backup and Storage for Lisp Image Sata (**Ed NB:** in
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

### Usage Cases, POSIX/UNIX Host Information (HostKR) Support

* Portable Host Identity for Systems Management Tasks

### Usage Cases, POSIX/UNIX Toolchain Information (ToolKR) Support

* **Ed. NB:** Syscall information library (BSD)
* **Ed. NB:** ELF (Non-Itanium) ABI support (Implementation-Specific
  Support for Typically OS-Specific Bytecode Library Support);
  `dlopen()` and related functions (host/other libc)


## Design Documentation - API (Notes)

### OS Process Initialization, POSIX/UNIX Hosts - `fork()` and `exec()`

* NB: Concerns pursuant to/after process `fork()`( **Ed. NB,** Generalized
      Context: _Resource Management in the Forked Process_)
    * Generalized Concerns - Close I/O streams in Forked Process
      (prevent ambiguous PTY I/O)
    * Implementation-Specific Concerns - Provide Generalized "Close On
      Exec" Support, in Implementation
    * Implementation-Specific Concerns - Provide Optional "External
      Object Handling" in Forked Process (e.g object deinitialization
      and deallocation, resource release, and optional bytecode library
      unloading)

* NB: Concerns pursuant to implementation of the fork/exec call form:
    * Provide string-to-temporary-file management within the forking
      process (temporary file creation, optional post-process cleanup)
      and the forked process (open file and `du(p)` for standare/other
      descriptor I/O; close and delete file before normal exit)
    * TBD: Debugger-over-IPC support (Protocol, REPL UI) for interactive
      management of exceptional situations in the forked process (NB,
      Usage Case: Debugger I/O before process exec)


## References

* SUSv4, X/Open
* BSD Manual Pages
* Linux Manual Pages

[sp]: http://www.jclark.com/sp/
[libxml2]: http://xmlsoft.org/
[bmake]: http://www.crufty.net/help/sjg/bmake.html
[gmake]: http://www.gnu.org/software/make/
