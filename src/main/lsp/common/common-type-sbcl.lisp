;; common-type-sbcl.lisp - Implementation-specific type system reflection, SBCL
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Local API and Implementation
;;
;;------------------------------------------------------------------------------

;; FIXME - test in CMUCL

(in-package #:ltp/common)


#+SBCL
(declaim (inline symbol-type)
         (sb-ext:maybe-inline  find-lvar))


;; NB/QA SBCL 1.5.2.8-5128df17d amd64 FreeBSD local build w/ safepoint etc
;; entering SBCL LDB e.g during SWANK::LOAD-FILE or LOAD w/o SLIME
;; when FIND-LVAR is declared expressly INLINE, here
;;
;; ... after emitting a warning that FIND-LVAR could not be inlined
;;
;; workaround: declare the function MAYBE-INLINE

#+SBCL
(defun find-lvar (name env)
  ;; NB: Roughly analogous to (sb-int:info :variable :type name)
  ;; but applied for purpose of accessing the lexical environmnent ENV
  (declare (type symbol name)
           (type sb-kernel:lexenv env)
           (values &optional sb-c::lambda-var))
  (let ((spec (assoc name (sb-c::lexenv-vars env)
                     :test #'eq)))
    (cond
      ((consp spec) (cdr (the cons spec)))
      ((sb-c::null-lexenv-p env) (values))
      (t (find-lvar name (sb-c::lexenv-parent env))))))


;; (defmacro frob-env (&environment env) `(values ,env))

;; (find-lvar '*standard-output* (frob-env))
;; => no values
;; ^ see SYMBOL-TYPE

;; (let ((a 5)) (find-lvar 'a (frob-env)))

;; (let ((a 5)) (let ((b 2)) (find-lvar 'a (frob-env))))

(define-condition no-binding (cell-error)
  ()
  (:report
   (lambda (c s)
     ;; Assumption: If this error occurs, the debugger should be able to
     ;; provide information about the lexical environment in which the
     ;; error occurred
     ;;
     ;; TBD: Portable representation for implementation-specific lexical
     ;; environment definitions, in LTP/Common
     (format s  "~<No binding availble for ~s~>"
             (cell-error-name c)))))

;; (error 'no-binding :name (make-symbol "_FrobP"))


#+SBCL
(defun symbol-type (s env)
  ;; NB: Broadly analogous to SYMBOL-VALUE but providing access to some
  ;; information with regrads to runtime type system reflection
  ;;
  ;; See also: SPECIFIER-TYPE in CMUCL, SBCL
  (declare (type symbol s)
           (type sb-kernel:lexenv env)
           (values type-designator &optional)
           (inline find-lvar))
  ;; First search ENV for a binding onto S
  (let ((lvar (find-lvar s env)))
    (cond
      (lvar
       (sb-kernel:type-specifier (sb-c::lambda-var-type lvar)))
      ((boundp s)
       ;; NB: SB-INT:INFO may not be checking whether the symbol
       ;; is bound globally. The function appears to return T for any
       ;; unbound symbol provided to it.
       ;;
       ;; So, this function works around that by calling SYMBOL-BOUNDP
       ;;
       ;; Susbq, return information for S, from the global environment.
       ;;
       ;; Assumption The globl binding state of the symbol has been
       ;; verified, previous to the following.
       ;;
       (sb-kernel:type-specifier (sb-int:info :variable :type s)))
      (t
       (error 'no-binding :name s)))))


;; (symbol-type '*standard-output* SB-CLTL2::*NULL-LEXENV*)
;; => STREAM

;; (symbol-type '*standard-output* (SB-KERNEL:MAKE-NULL-LEXENV))
;; => STREAM

;; Expect Failure:
;; (symbol-type (make-symbol "frob") (SB-KERNEL:MAKE-NULL-LEXENV))

;; NB using the following macro
;; (let ((s (make-symbol "frob"))) (setf (symbol-value s) nil) (symbol-type* s))
;; => T



;; (symbol-type '*standard-output* (frob-env))
;; => STREAM

;; (let ((a 1)) (declare (type (mod 2) a)) (symbol-type 'a (frob-env)))
;; => BIT ;; NB type was reduced by the compiler

;; (let ((a 1)) (declare (type (mod 10) a)) (symbol-type 'a (frob-env)))
;; => (MOD 10)

;; (let ((a 1)) (symbol-type 'a (frob-env)))
;; => T


#+SBCL
(defmacro symbol-type* (s &environment env)
  ;; NB: This has been defined as a macro,
  ;; in order to access the lexical environment ENV implicitly
  `(symbol-type ,s ,env))


;; (symbol-type* '*standard-output*)
;; => STREAM

;; (let ((a 1)) (declare (type (mod 2) a)) (symbol-type* 'a))
;; => BIT ;; NB type was reduced by the compiler

;; (let ((a 1)) (declare (type (mod 10) a)) (symbol-type* 'a))
;; => (MOD 10)

;; (let ((a 1)) (symbol-type* 'a))
;; => T



;; -- sketch-wk --------------------

(eval-when ()

;; NB: Global symbols

;; See also: LOCALLY ...

#+SBCL
(sb-int:info :variable :type '*standard-output*)

#+SBCL
(sb-int:info :variable :type (make-symbol "frob"))

#+SBCL
(sb-int:info :variable :where-from '*standard-output*)

;; (sb-kernel::classoid-class-info (sb-int:info :variable :type '*standard-output*))

;; (sb-kernel::type-class-info  (sb-int:info :variable :type '*standard-output*))

;; (SB-KERNEL:BUILT-IN-CLASSOID-TRANSLATION (sb-int:info :variable :type '*standard-output*))


;; (sb-kernel:ctype-of ....) ;; operates on objects


) ;; EVAL-WHEN


(eval-when ()

  (defmacro frob-env (&environment env)
    `(values ,env))

  ;; (frob-env)

  ;; (describe (class-of (frob-env)))

  ;; (type-of (frob-env))
  ;; => SB-KERNEL:LEXENV

  ;; NB: SB-C:*LEXENV*

  ;; (sb-c::lexenv-type-restrictions  ??)

  (let ((a 1))
    (declare (type (mod 10) a))
    (frob-env))

  ;; NB: LEXENV-VARS



  (let ((a 1))
    (declare (type (mod 10) a))
    ;; NB: (SB-C::LEXENV-PARENT INST)
    (cdar (sb-c::lexenv-vars (frob-env))))

  (let ((a 1))
    (declare (type (mod 10) a))
    ;; NB: (SB-C::LEXENV-PARENT INST)
    (sb-c::lambda-var-type (cdar (sb-c::lexenv-vars (frob-env)))))


  (let ((a 1))
    (declare (type (mod 10) a))
    ;; NB: (SB-C::LEXENV-PARENT INST)
    ;;
    (sb-c:primitive-type (sb-c::lambda-var-type (cdar (sb-c::lexenv-vars (frob-env))))))


  (let ((a 1))
    (declare (type (mod 10) a))
    (sb-kernel:type-specifier
     (sb-c::lambda-var-type (cdar (sb-c::lexenv-vars (frob-env))))))


) ;; EVAL-WHENx
