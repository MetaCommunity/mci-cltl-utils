;; ext-pkg.lisp - defpackage for ASDF extensions [MCi]

(in-package #:cl-user)


(defpackage #:info.metacommunity.cltl.sysdef.asdf
  (:nicknames #:mcicl.sysdef.asdf)
  (:use #:info.metacommunity.cltl.utils
        #:asdf
        #:cl)
  (:shadowing-import-from
   #:asdf/find-system
   #:register-system
   )
  (:export
   #:component-designator
   
   #:component-condition
   #:component-condition-component
   
   #:component-location-condition
   #:component-location-condition-location

   #:system-not-found

   #:find-component*
   #:find-system*

   ;;; moved into defsys-ex.lisp
   ;; #:resource
   ;; #:resource-file
   ;; #:resource-module
   ;; #:resource-system
   
   #:alias-system-alias-to
   #:alias-system
   #:make-alias-system
   #:register-alias-system
  ))
