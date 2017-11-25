;; ext-alias.lisp - system aliasing extension for ASDF
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;; 
;; Contributors: Sean Champ - Initial Interface and Implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp-asdf-utils)

(defgeneric alias-system-alias-to (system))
(defgeneric (setf alias-system-alias-to) (component system))

(defclass alias-system (system*)
  ((alias-to
    ;; source of alias - should denote an existing system definition
    :initarg :alias-to
    :type (or simple-string symbol component)
    :accessor alias-system-alias-to)))

(defmethod print-object ((object alias-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S => ~S"
            (component-name object)
            (alias-system-alias-to object))))

(defun make-alias-system (to from)
  (let* ((%to (coerce-name to))
         (%from (coerce-name from)))
    (make-instance 'alias-system
                   :name %to
                   :alias-to %from)))

(defun register-alias-system (to from)
  (let* ((s (make-alias-system to from)))
    (register-system s)))

(defmethod operate :around ((operation operation) (component alias-system)
                            &rest args &key &allow-other-keys)
  (let* ((to (alias-system-alias-to component))
         (%to (etypecase to
                (component to)
                (t (find-system to)))))
    ;; FIXME This does not perform output-file aliasing for 'TO' under 'COMPONENT'
    (cond 
      (args (apply #'operate operation %to args))
      (t  (operate operation %to)))))
