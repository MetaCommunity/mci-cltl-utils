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
  :components 
  ((:file "utils-package")
   (:file "fn-utils"
	  :depends-on ("utils-package"))
   (:file "condition-utils"
	  :depends-on ("utils-package"))
   (:file "asdf-utils"
	  :depends-on ("condition-utils"
		       "utils-package"))
   ))
