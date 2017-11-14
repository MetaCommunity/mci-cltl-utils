;; utils-package.lisp
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
;; DESCRIPTION
;;
;; Conventional `defpackage'
;;
;; SEE ALSO
;;
;; [ANSI CL]
;;

(in-package #:cl-user)


(defpackage #:utils.ltp.thinkum.space
  (:nicknames #:utils #:utils.ltp)
  (:use #:asdf #:cl)

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
   #:entity-condition
   #:entity-condition-name
   #:entity-not-found
   #:redefinition-condition
   #:redefinition-condition-previous-object
   #:redefinition-condition-new-object
   #:container-condition
   #:container-condition-container

   #:compute-output-stream
   #:compute-input-stream

   #:setf-function-designator
   #:lambda-function-designator
   #:function-designator
   #:compute-function
   #:with-safe-frefs
   #:function-name

   #:call-next-method*
   #:slot-value*
   #:when-slot-init

   #:stream-designator
   #:format-control

   #:defconstant*
   #:compilation-condition
   #:compilation-condition-function-name
   #:compilation-condition-lambda-form
   #:warnings-during-compile
   #:errors-during-compile
   #:compile*
   #:symbol-status

   #:print-name
   #:print-label
   #:object-print-name
   #:object-print-label
   #:pretty-printable-object
   #:format-label
   #:print-hash-table
   #:associative-object
   #:object-name

   #:with-optimization
   #:with-tail-recursion

   #:compilation-condition
   #:compilation-warning
   #:simple-compilation-warning
   #:compilation-error
   #:simple-compilation-error
   #:compile*
   
   #:push-last
   #:simplify-vector
   #:do-vector
   #:simplify-string
   #:string-position
   #:null-string
   #:string-null-p
   #:array-dimension-designator
   #:array-length
   #:split-string-1
   #:split-string

   #:character-code
   #:code-in-range
   #:code=
   #:name-star-char-p
   #:name-char-p
   #:read-name-string
   #:read-characters
   ))

