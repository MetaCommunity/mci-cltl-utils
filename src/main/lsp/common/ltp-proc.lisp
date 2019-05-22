;; ltp-proc.lisp -
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/common)

;; NB: Better than trying to mangle "Shell Text" with Emacs' naive comint API

(defun run-with-pipe (&rest procspec)
  "Create a process for each PROCSPEC, ensuring each process' normative
output descriptor is bound to the subsequent process' normative input
descriptor"

  ;; Notes
  ;;
  ;; * process specifiers and Process Descriptors - T.D (LTP)
  ;; - NB stderr descriptor & run-with-pipe
  ;; - TBD Semantics for descripror/file mapping (nin, nout, errout)
  ;;   - NB: Portable I/O Descriptor Mapping (cf. CP/M?? and DOS and MSVC I/O, and POSIX systems)
  ;; - TBD Environment specifiers; note semantics of DJB envdir
  ;; - TBD Cmd parameter mapping for process specifiers [X] - Syntax
  ;;   - argv[0]
  ;;   - argv[1+]
  ;; - TBD Optional/Feature Dispatchable Syntax for Process 'Nice' priority
  ;; - Executor UID, GID (Principal UID/GID and Process UID/GID - onto POSIX terms) and OS permission models (SUSv4 and other)
  ;; - [X] Defun LTP/EO:EXTERN-STRING & subsq. See also: TL-Contrib//CL+J
  ;;  - TD: Extenal ABI Environment (??) Specifiers (C, C++, Java, GLib)
  ;;  - [X] TBD: SINGLETON-CLASS (All slots class-allocated; analogy onto Java static forms; impl-specific extensions for specialized storage - SBCL and the libc stack, CCL & subsq.; convenience methods and functional API)
  ;;    - SINGLETON-CLASS application in LTP/EO ABI Environment Definitions
  ;;    - C ABI [SINGLETON-CLASS]
  ;;    - C++ ABI [ABSTRACT-CLASS - Subsume ABIs for LLVM and GCC]
  ;;    - JLib ABI [SINGLETON-CLASS] - Onto CL+J (??) NB Java Embedded Invocation ABI and other JNI API options
  ;;    - GLib ABI [SINGLETON-CLASS] - Onto C ABI
  ;;    - gtkmm ABI [ABSTRACT CLASS] - Onto GLib ABI, C++ ABI
  ;; - Documentation, nothing too ostentatious in devsrc
  ;;
  ;; * Process Objects
  ;;   - process reflection - operns from external to a process' OS environ (vis a vis the UNIX 'ps' util)
  ;;     - process state
  ;;     - process principal actor - host user descriptors - visible UID/GID (POSIX hosts); consult Cygwin documentation (Microsoftq Windows PC systems)
  ;;     - process cmdline
  ;;     - process PID, pgrp, and (may be unavl) controlling TTY
  ;;   - wait, waitpid (POSIX environments)
  ;;     - NB: Prototyping may require a multithreaded Lisp implementation (arm/other machine arch)
  ;;   - process I/O descriptors
  ;;     - dup ... (SBCL, POSIX; LTP EO Protyping @ OS. cf. SUSv4, et.c) (POSIX environments)
  ;;     - streams - bare FD i/o stream, time-annotated FD i/o stream
  ;;     - file I/O and OS process objects
  ;;       - TBD detecting when a stream is open on a file, in "The Lisp" simultaneous to another open FD onto "The same file," in any process initialized from "The Lisp"
  ;;   - TTYs, TERM, and termios
  ;;   - functional API and interactive programming examples
  ;;
  ;; * QA - Usage Cases
  ;; - ltp-proc/ltp-cmd and git orchestration (gen-diffs.el example)
  ;; - ltp-proc/ltp-cmd and parameterization for XML tools (DocBook, TEI P5, other)
  ;; - arbitrarily toying around in SLIME (??)
  ;; - LTP model tools
  )
