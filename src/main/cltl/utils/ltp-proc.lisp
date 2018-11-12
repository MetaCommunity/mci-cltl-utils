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

(in-package #:ltp-utils)

;; NB: Better than trying to mangle "Shell Text" with Emacs' naive comint API

(defun run-with-pipe (&rest procspec)
  "Create a process for each PROCSPEC, ensuring each process' normative
output descriptor is bound to the subsequent process' normative input
descriptor"

  ;; Notes
  ;;
  ;; dup ... (SBCL, POSIX; LTP EO Protyping @ OS. cf. SUSv4, et.c)
  ;;
  ;; process specifiers and Process Descriptors - T.D (LTP)
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
)
