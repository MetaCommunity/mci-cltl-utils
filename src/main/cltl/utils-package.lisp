
(in-package #:cl-user)


(defpackage #:info.metacommunity.cltl.utils
  (:nicknames #:utils)
  (:use #:asdf #:cl)

  (:export
   #:file-name
   #:file-designator

   #:simple-style-warning
   #:simple-program-error

   #:with-safe-frefs
   )

  #+ASDF
  (:export
   #:component-designator

   #:component-condition
   #:component-condition-component

   #:component-container-condition
   #:component-container-condition-container

   #:module-component-absent
   #:system-not-found

   #:find-component*
   #:find-system*
  )
  )

