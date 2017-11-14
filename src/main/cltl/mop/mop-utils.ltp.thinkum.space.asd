;; info.metacommunity.cltl.mop.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:utils-system
    (:use #:asdf #:cl)))

(in-package #:utils-system)


(defsystem #:info.metacommunity.cltl.utils.mop
  ;; :description ""
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :depends-on (#:info.metacommunity.cltl.utils
               #:closer-mop
               #:bordeaux-threads
               )
  :components 
  ((:file "mop-pkg")
   (:file "mop-utils"
          :depends-on ("mop-pkg"))
   (:file "aclass"
          :depends-on ("mop-utils" "mop-pkg"))
   ))
