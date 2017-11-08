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