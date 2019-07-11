;; acc-package.lisp - Package Definition for ltp-base-class-acc system
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:cl-user)

(defpackage #:ltp/base-class/accessor
  (:nicknames #:ltp.base-class.accessor)
  (:use #:ltp/common/mop #:ltp/common #:cl)
  #+SBCL
  (:shadowing-import-from
   ;; FIXME: Port to CMUCL, other implementations using PCL
   ;;
   ;; Note remarks with regards to DEFSIGNATURE, in acc-gen.lisp
   #:sb-pcl #:slot-definition-class)
  (:export
   #:write-accessors
   #:write-accessors-for
   ))
