;; type-utils.lisp
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/common)

(deftype file-name ()
  '(or string pathname))

(deftype file-designator ()
  '(or string pathname file-stream))

(deftype class-designator ()
  '(or symbol class))

(deftype type-designator ()
  '(or symbol class (cons symbol t)))


(defun* compute-class (ident &optional (errorp t)
                             environment)
  (declare (type class-designator ident)
           (values (or class null) &optional))
  (etypecase ident
    (symbol (find-class ident errorp environment))
    (class ident)))

;; trivial tests for COMPUTE-CLASS
;; (compute-class 'string)
;; (compute-class (find-class 'ratio))
;; (compute-class (class-of (1+ most-positive-fixnum)))

(deftype unsigned-fixnum ()
  '(integer 0 #.most-positive-fixnum))


;; Novel but unused
#-(and)
(defun unsigned-size-of (uint)
  ;; NB: This does not align to either fixnum or bignum boundaries
  (declare (type (integer 0) uint)
           (values (integer 0) &optional))
  (values (ceiling (log uint 2))))

;; (unsigned-size-of (expt 2 8))
;; => 8
