;; ltp-common-mop.asd                                               -*- lisp -*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2019 Sean Champ and others. All rights reserved.
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
  (unless (find-package '#:ltp-common-system)
    (defpackage #:ltp-common-system
      ;; note package reuse - package is defined originally in ltp-common.asd
      (:use #:asdf #:cl))))

(in-package #:ltp-common-system)


(defsystem #:ltp-common-singleton
  ;; :description ""
  :version "1.0"
  ;;  :homepage TBD
  :license "spdx:EPL-1.0"
  :depends-on (#:ltp-common-mop
               #:closer-mop
               #:ltp-common
               )
  :components
  ((:file "singleton")
   ))
