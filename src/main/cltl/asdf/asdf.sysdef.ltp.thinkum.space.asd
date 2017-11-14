;; asdf.sysdef.ltp.thinkum.space.asd			       	-*-lisp-*-
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:utils-system.ltp
    (:use #:asdf #:cl)))

(in-package #:utils-system.ltp)


(defsystem #:asdf.sysdef.ltp.thinkum.space
  ;; :description ""
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :depends-on (#:utils.ltp.thinkum.space)
  :components 
  ((:file "ext-pkg")
   (:file "ext-utils"
          :depends-on  ("ext-pkg"))
   (:file "ext-star"
          :depends-on ("ext-pkg"))
   (:file "ext-alias"
          :depends-on ("ext-pkg" 
                       "ext-star"))
   ))
