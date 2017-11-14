;; mop-utils.ltp.thinkum.space.asd
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
  (defpackage #:system.utils.ltp.thinkum.space
      (:nicknames #:utils-system.ltp)
    (:use #:asdf #:cl)))

(in-package #:utils-system.ltp)


(defsystem #:mop-utils.ltp.thinkum.space
  ;; :description ""
  :version "1.0"
  ;;  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
  ;;  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :depends-on (#:utils.ltp.thinkum.space
               #:closer-mop
               #+LTP_PROTOTYPES #:bordeaux-threads
               )
  :components 
  ((:file "mop-pkg") ;; c2mop integration
   (:file "mop-utils" ;; validate-[super]class (convenience macro)
          :depends-on ("mop-pkg"))
   #+LTP_PROTOTYPES
   (:file "aclass"
          :depends-on ("mop-utils" "mop-pkg"))
   ))
