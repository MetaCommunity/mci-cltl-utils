;; singleton.lisp
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

(defpackage #:ltp/common/mop/singleton
  (:nicknames #:ltp.common.mop.singleton)
  (:use #:ltp/common/mop
        #:ltp/common
        #:cl))

(in-package #:ltp/common/mop/singleton)



(defclass singleton-class (standard-object standard-class)
  ())


(defclass singleton-slot-definition (standard-slot-definition)
  ;; TBD: SLOT-DEFINITION (??)
  ())


(defclass singleton-direct-slot-definition
    (singleton-slot-definition standard-direct-slot-definition)
  ())



(defclass singleton-effective-slot-definition
    (singleton-slot-definition standard-effective-slot-definition)
  ())

;; MOP Interop ...

;; Applications TD
;; - Use for internal representation of "Application Personalities" in
;;   the Lisp Environment
;;
;; - Use for internal representation of ABIs
