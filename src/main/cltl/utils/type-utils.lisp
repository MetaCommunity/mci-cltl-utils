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

(in-package #:utils.ltp)

(deftype file-name ()
  `(or string pathname))

(deftype file-designator ()
  `(or string pathname file-stream))

(deftype class-designator ()
  '(or symbol class))

(deftype type-designator ()
  '(or symbol class (cons symbol t)))
		 

(defun compute-class (ident &optional (errorp t)
                              environment)
  (declare (type class-designator ident)
           (values (or class null)))
  (etypecase ident
    (symbol (find-class ident errorp environment))
    (class ident)))

;; trivial tests for COMPUTE-CLASS
;; (compute-class 'string)
;; (compute-class (find-class 'ratio))
;; (compute-class (class-of (1+ most-positive-fixnum)))

(deftype unsigned-fixnum ()
  '(integer 0 #.most-positive-fixnum))
