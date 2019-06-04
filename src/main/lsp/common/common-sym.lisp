;; common-sym.lisp - utilities for Common Lisp Symbols
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


(in-package #:ltp/common)


(defun* symbol-status (s) ; FIXME move to (new) common-sym.lisp

  ;; Ed. NB: used in PRINT-NAME (SYMBOL STREAM)
  ;; and in PRINT-LABEL (SYMBOL STREAM)
  (declare (type symbol s)
	   (values symbol (or package null)))
  (let ((pkg (symbol-package s)))
    (cond
      (pkg
       (multiple-value-bind (s status)
	   (find-symbol (symbol-name s)
			pkg)
	 (declare (ignore s))
	 (values status pkg)))
      (t (values nil nil)))))

;; (symbol-status 's)
;; => :INTERNAL, #<PACKAGE "LTP/COMMON">

;; (symbol-status 'print)
;; => :EXTERNAL, #<PACKAGE "COMMON-LISP">

;; (symbol-status 'ltp/common::print)
;; => :EXTERNAL, #<PACKAGE "COMMON-LISP">

;; (symbol-status '#:foo)
;; => NIL, NIL


(deftype package-designator ()
  '(or symbol string package))

(defun* package-exports-symbols (pkg)
  (declare (type package-designator pkg)
           (values list &optional))
  (let ((buffer
         (make-array 8 :fill-pointer 0)))
    (declare (type (array t (*)) buffer))
    (do-external-symbols (s pkg  (coerce buffer 'list))
      (vector-push-extend s buffer 8))))

;; Test form (ad hoc):
;; (package-exports-symbols '#:c2mop)
