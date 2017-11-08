;; info.metacommunity.cltl.sysdef.asdf.asd		-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:utils-system
    (:use #:asdf #:cl)))

(in-package #:utils-system)


(defsystem #:info.metacommunity.cltl.sysdef.asdf
  ;; :description ""
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :depends-on (#:info.metacommunity.cltl.utils)
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
