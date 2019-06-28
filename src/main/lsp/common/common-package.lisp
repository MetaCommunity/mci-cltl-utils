;; common-package.lisp
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------
;;

(in-package #:cl-user)


(defpackage #:ltp/common
  (:nicknames #:ltp.common)
  (:use #:cl)

  (:export
   #:format*
   #:with-gensym
   #:with-symbols
   #:intern*
   #:intern-formatted

   #:file-name
   #:file-designator
   #:class-designator
   #:type-designator
   #:compute-class

   #:unsigned-fixnum

   #:simple-style-warning
   #:simple-program-error
   #:format-condition
   ;; - FIXME: Move the following into supplemental system definitions
   #:entity-condition ;; NB is not a cell-error
   #:entity-condition-name
   #:entity-not-found ;; NB see also container-condition
   #:redefinition-condition
   #:redefinition-condition-previous-object
   #:redefinition-condition-new-object
   #:container-condition
   #:container-condition-container

   #:compute-output-stream
   #:compute-input-stream

   #:setf-function-designator ;; NB typing & knowledge representation for Common Lisp programs
   #:lambda-function-designator
   #:function-designator
   #:compute-function
   #:with-safe-frefs ;; NB Usability @ ASDF extensions
   #:function-name

   ;; FIXME Consider moving the following three forms into the LTP common-mop system
   #:call-next-method*
   #:slot-value*
   #:when-slot-init

   #:stream-designator
   #:format-control

   #:defconstant*

   #:with-optimization ;; FIXME see also WITH-COMPILATION-UNIT (SBCL, ...)
   #:with-tail-recursion ;; [...]

   #:symbol-status

   #:package-designator
   #:package-exports-symbols
   #:package-exports-symbols-if

   #:defun*
   ;; #:labels*

   #:push-last
   #:nappend
   #:npushl
   #:map-plist

   #:simplify-vector
   #:do-vector
   #:simplify-string
   #:string-position
   #:null-string
   #:string-null-p
   #:array-dim
   #:array-index
   #:array-length
   #:typed-svector
   #:typed-vector
   #:split-string-1
   #:split-string

   #:featurep
   #:mk-lf

   #:compile-condition
   #:lambda-compile-condition
   #:lambda-compile-condition-form
   #:lambda-compile-error
   #:lambda*

   ;; FIXME: Move the following to another system - ltp-common-parse e.g
   ;; Note the fairly XML-oriented nature of the terms, here.
   ;;
   ;; reuse in ltp-common-index
   #:character-code
   #:code-in-range
   #:code=
   #:code-name-start-char-p #:char-name-start-char-p
   #:code-name-char-p #:char-name-char-p
   #:read-name-string
   #:read-characters

   ;; FIXME: Move the following to another system - ltp-common-pprint e.g
   #:print-name
   #:print-label
   #:object-print-name
   #:object-print-label
   #:pretty-printable-object
   #:format-label
   #:print-hash-table
   ;; #:associative-object ;; moved to ../mop/aclass/
   ;; #:object-name

   ))

