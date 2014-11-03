
(in-package #:cl-user)


(defpackage #:info.metacommunity.cltl.utils
  (:nicknames #:utils)
  (:use #:asdf #:cl)

  (:export
   #:format*
   #:with-gensym
   #:intern*
   #:intern-formatted

   #:file-name
   #:file-designator
   #:class-designator
   #:compute-class

   #:simple-style-warning
   #:simple-program-error
   #:format-condition

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

   #:with-optimization
   #:with-tail-recursion
   
   #:push-last
   #:simplify-string
   #:string-position
   #:null-string
   #:string-null-p
   #:array-dimension-designator
   #:split-string-1
   #:split-string

   #:character-code
   #:code-in-range
   #:code=
   #:name-star-char-p
   #:name-char-p
   #:read-name-string
   )

  #+ASDF
  (:export
   #:component-designator

   #:component-condition
   #:component-condition-component

   #:component-location-condition
   #:component-location-condition-location

   #:module-component-absent
   #:system-not-found

   #:find-component*
   #:find-system*

   ;;; moved into defsys-ex.lisp
   ;; #:resource
   ;; #:resource-file
   ;; #:resource-module
   ;; #:resource-system
  )
  )

