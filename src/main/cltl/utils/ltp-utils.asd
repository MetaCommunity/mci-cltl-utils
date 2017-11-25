;; ltp-utils.asd - generic utilitiess system, LTP                     -*-lisp-*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;; 
;; Contributors: Sean Champ  - Initial implementation
;;
;;------------------------------------------------------------------------------
;;
;; DESCRIPTION
;;
;; The generic utils system - system definition in ASDF syntax. Published by the
;; Lisp Tools Project at Thinkum Labs.
;;
;; SEE ALSO
;;
;; * CLOCC [Project]
;; * Alexandria [Project]

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:ltp-utils-system
    ;; note package reuse - package ltp-utils-system
    (:use #:asdf #:cl)))

(in-package #:ltp-utils-system)


(defsystem #:ltp-utils
  :description 
  "Generic utilities for programming with Common Lisp and ASDF"
  :version "1.0"
;;  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
;;  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :components 
  ((:file "ltp-utils-package")
   (:file "macro-utils"
	  :depends-on ("ltp-utils-package"))
   (:file "type-utils"
	  :depends-on ("ltp-utils-package"))
   (:file "clos-utils"
	  :depends-on ("macro-utils"))
   (:file "fn-utils"
	  :depends-on ("ltp-utils-package"))
   (:file "condition-utils"
	  :depends-on ("ltp-utils-package"))
   (:file "stream-utils"
          :depends-on ("ltp-utils-package"))
   (:file "misc-utils" 
          :depends-on ("stream-utils" "macro-utils" "condition-utils"))
   (:file "print-utils"
	  :depends-on ("misc-utils" "fn-utils"))
   (:file "opt-utils"
	  :depends-on ("ltp-utils-package" "macro-utils"))
   (:file "seq-utils"
	  :depends-on ("opt-utils"))
   (:file "reader-utils"
	  :depends-on ("type-utils" "stream-utils"))
   ))
