;; ltp-common.asd - generic utilitiess system, LTP                    -*-lisp-*-
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
;; A generic utilities system, providing a number of reusable forms for
;; Common Lisp progrms. The types of forms provided in this system
;; include:
;;
;; - Functions
;; - Macros
;; - Type Definitions
;; - Condition Types
;;
;; SEE ALSO
;;
;; * CLOCC [Project]
;; * Alexandria [Project]

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:ltp-common-system
    ;; note package reuse - package ltp-common-system
    (:use #:asdf #:cl)))

(in-package #:ltp-common-system)


(defsystem #:ltp-common
  :description
  "Generic utilities for programming with Common Lisp"
  :version "2.0"

  ;; FIXME update for MCi (deprecation) => [TL] LTP (adoption)

;;  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
;;  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :components
  ((:file "common-package")
   (:file "common-macro"
	  :depends-on ("common-package"))
   (:file "common-type"
	  :depends-on ("common-package" "common-lambda"))
   (:file "common-clos"
	  :depends-on ("common-macro"))
   (:file "common-fn"
	  :depends-on ("common-package" "common-lambda"))
   (:file "common-condition"
	  :depends-on ("common-package"))
   (:file "common-stream"
          :depends-on ("common-package" "common-lambda"))
   (:file "common-misc"
          :depends-on ("common-stream" "common-macro" "common-condition"
                                       "common-lambda"))
   (:file "common-print"
          ;; FIXME see remarks in common-packge.lisp
	  :depends-on ("common-misc" "common-fn" "common-lambda"))
   (:file "common-opt"
          ;; FIXME see remarks in common-packge.lisp
	  :depends-on ("common-package" "common-macro"))
   (:file "common-seq"
	  :depends-on ("common-macro"))
   (:file "common-string"
          :depends-on ("common-macro" "common-type" "common-misc"
                                      "common-lambda"
                                      "common-opt" ;; WITH-TAIL-RECURSION [TBD]
                                      ))
   (:file "common-reader"
	  :depends-on ("common-type" "common-stream"))

   (:file "common-lambda"
          :depends-on ("common-macro" ;; DEFCONSTANT*
                       "common-seq" ;; NPUSHL
                       "common-condition" ;; SIMPLE-STYLE-WARNING
                       ))
   ))
