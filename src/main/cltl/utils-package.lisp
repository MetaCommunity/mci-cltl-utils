
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


   #:setf-function-designator
   #:lambda-function-designator
   #:function-designator
   #:compute-function
   #:with-safe-frefs

   #:call-next-method*
   #:slot-value*
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

