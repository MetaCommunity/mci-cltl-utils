;; info.metacommunity.cltl.utils.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:utils-system
    (:use #:asdf #:cl)))

(in-package #:utils-system)


(defsystem #:info.metacommunity.cltl.utils
  :description 
  "Generic utilities for programming with Common Lisp and ASDF"
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-cltl-utils"
  :license "https://github.com/MetaCommunity/mci-cltl-utils/blob/master/LICENSE"
  :components 
  ((:file "utils-package")
   (:file "macro-utils"
	  :depends-on ("utils-package"))
   (:file "type-utils"
	  :depends-on ("utils-package"))
   (:file "clos-utils"
	  :depends-on ("utils-package"
		       "macro-utils"))
   (:file "fn-utils"
	  :depends-on ("utils-package"))
   (:file "condition-utils"
	  :depends-on ("utils-package"))
   (:file "asdf-utils"
	  :depends-on ("condition-utils"
		       "utils-package"))
   ))
