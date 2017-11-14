;; utils.ltp.thinkum.space.asd - generic utils system, LTP            -*-lisp-*-
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
  (defpackage #:system.utils.ltp.thinkum.space
    ;; note package reuse - utils-system.ltp
    (:nicknames #:utils-system.ltp)
    (:use #:asdf #:cl)))

(in-package #:utils-system.ltp)


(defsystem #:utils.ltp.thinkum.space
  :description 
  "Generic utilities for programming with Common Lisp and ASDF"
  :version "1.0"
;;  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
;;  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :components 
  ((:file "utils-package")
   (:file "macro-utils"
	  :depends-on ("utils-package"))
   (:file "type-utils"
	  :depends-on ("utils-package"))
   (:file "clos-utils"
	  :depends-on ("macro-utils"))
   (:file "fn-utils"
	  :depends-on ("utils-package"))
   (:file "condition-utils"
	  :depends-on ("utils-package"))
   (:file "stream-utils"
          :depends-on ("utils-package"))
   (:file "misc-utils" 
          :depends-on ("stream-utils" "macro-utils" "condition-utils"))
   (:file "print-utils"
	  :depends-on ("misc-utils" "fn-utils"))
   (:file "opt-utils"
	  :depends-on ("utils-package" "macro-utils"))
   (:file "seq-utils"
	  :depends-on ("opt-utils"))
   (:file "reader-utils"
	  :depends-on ("type-utils" "stream-utils"))
   ))
