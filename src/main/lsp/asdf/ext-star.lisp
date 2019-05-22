;; ext-star.lisp - platform-specific subclasses for components
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Interface and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp-asdf-utils)

(defclass component* (component)
  ())

(defclass module* (component* module)
  ())

(defclass system* (component* system)
  ;; cf. :CLASS initarg to DEFSYSTEM
  ())

(defvar *component*)
(defvar *operation*)

(defmethod operate :around ((operation operation)
                            (component component*)
                            &key &allow-other-keys)
  ;; make OPERATION and COMPONENT avaialble to more specialized methods
  (let ((*component* component)
        (*operation* operation))
    (call-next-method)))
