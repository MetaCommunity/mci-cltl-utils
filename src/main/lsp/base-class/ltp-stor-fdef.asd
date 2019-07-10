;; ltp-sbase-class-fdef.asd - Function Definitions after DEFSTRUCT    -*-lisp-*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ  - Initial implementation
;;
;;------------------------------------------------------------------------------
;;

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:ltp-common-system
    ;; note package reuse - package ltp-common-system
    (:use #:asdf #:cl)))

(in-package #:ltp-common-system)


(defsystem #:ltp-base-class-fdef
  :license "spdx:EPL-1.0"

  :depends-on (#:ltp-common-mop
               #:ltp-common)

  :components
  (:file "fdef-package")
  (:file "fdef-acc"
         :depends-on ("fdef-package"))
  )
