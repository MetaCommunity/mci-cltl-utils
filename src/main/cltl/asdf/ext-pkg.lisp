;; ext-pkg.lisp - defpackage for ASDF extensions [MCi]
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;; 
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------

(in-package #:cl-user)


(defpackage #:asdf.sysdef.ltp.thinkum.space
  (:nicknames #:asdf.sysdef.ltp)
  (:use #:utils.ltp.thinkum.space
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

   ;;; moved into ../utils/defsys-ex.lisp
   ;; #:resource
   ;; #:resource-file
   ;; #:resource-module
   ;; #:resource-system
   
   #:alias-system-alias-to
   #:alias-system
   #:make-alias-system
   #:register-alias-system
  ))
