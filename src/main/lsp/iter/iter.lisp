;; iter.lisp - generalized formalisms for typed iteration
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation onto SB-THREAD
;;
;;------------------------------------------------------------------------------

(defpackage #:ltp/common/iter
  (:use #:ltp/common #:cl))


(in-package #:ltp/common/iter)

(defstruct (iterable
             (:constructor))
  (element-type
   t
   :type type-designator
   :read-only t))

(defstruct (sequence-iterable
             (:include iterable)
             (:constructor))
  (members
   nil
   :type sequence))

;; DNW ....

(defstruct (vector-iterable
             (:include sequence-iterable)
             (:constructor))
  #+FIXME
  (members
   #()
   :type vector))


(defstruct (simple-vector-iterable
             (:include vector-iterable)
             (:constructor %mk-simple-vector-iterable
                           (element-type members)))
  #+FIXME
  (members
   #()
   :type '(simple-array * (*))))


(defun make-simple-vector-iterable (members &key (element-type t))
  (%mk-simple-vector-iterable
   element-type
   (make-array (length members)
               :element-type (upgraded-array-element-type element-type)
               :initial-contents members)))

(defmacro do-simple-vector-iterable ((s whence &optional returnv) &body forms)
  ;; NB: This simple protocol may not provide support for iterable type
  ;; elements to be declared in a manner as to be available to a
  ;; compilation environment -- juxtaposed to any finalized class, in
  ;; any implementation as onto a STANDARD-CLASS
  ;;
  ;; Regardless, there may remain a question as to how provide a type
  ;; declratoin for a static evaluable form, within a macroexpansion,
  ;; for any object that is not available until the macroexpansion is
  ;; evaluated.

  (with-symbols (%s %whence %typ %do-main %returnv %forms)

    ;; NB: ANSI CL is fairly not good at supporting templates for source forms
    ;;
    ;; CL Macro syntax may seem however more advanced than anything
    ;; available in the immediate syntax of CPP macro declarations. The
    ;; call-time evaluation of CL macros provides some particularly
    ;; quirky limitations, however.

    `(let* ((,%whence ,whence)
            ;; TBD: How to  make the binding of %typ available at
            ;; compile time in a lexically scoped environment,
            ;; for any implementation inserting null-lexenv anywhere
            ;; arbitrarily
            ;;
            ;; define-compiler-macro does not in itself provide any
            ;; workaround for the intrinsic quirks/limitations of the
            ;; macroexpansion environment and subsequent breakage for
            ;; some not-evaluated-for-value forms
            (,%typ (iterable-element-type ,%whence)))
       (macrolet ((,%do-main ((,%s ,%whence ,%returnv) ,%forms)
                    (with-symbols (%%whence %memb %len %n)
                      `(let* ((,%%whence ,,%whence)
                              (,%memb (simple-vector-iterable-members ,%%whence))
                              (,%len (length (the simple-vector ,%memb))))
                         (dotimes (,%n ,%len ,,%returnv)
                           (let ((,,%s (svref ,%memb ,%n)))
                             ;; NB: An operation on the class of
                             ;; ,%WHENCE could seem to serve to work
                             ;; around the limitations of the
                             ;; macroexpansion evaluation environment,
                             ;; with the following DECLARE form --
                             ;; except that the binding of ,%WHENCE may
                             ;; be ignored by the compiler, even then,
                             ;; in the macroexpansion.
                             ;;
                             ;; For purpoes of development in this
                             ;; system, it might seem feasible to
                             ;; orchestrate the production of the
                             ;; evaluable form with a compiler
                             ;; macro operating on the iterable
                             ;; object itself, in production of the
                             ;; macroexpansion to the compiler. However --
                             ;; beyond any merely syntactic
                             ;; characteristics of the whole form
                             ;; provided ot the compiler macro -- the
                             ;; implementation of such an approach may
                             ;; serve to require an EVAL semantics on
                             ;; the expression denoting the iterable
                             ;; object - quite simply, evaluating it
                             ;; independent of the environment finally
                             ;; created by the compiler, moreover
                             ;; independent of the enviornment in which
                             ;; the form was initially conferred.
                             ;;
                             ;; Subsequently, perhaps the semantics of
                             ;; CMUCL source tranform definitions may
                             ;; seem somehow approachable for
                             ;; consideration, alternate to any concerns
                             ;; imposed by the creation of lexical
                             ;; environments in the compiler itself.
                             (declare (type ,(symbol-value (quote ,%typ)) ,,s))
                             ,@,%forms
                             ))))))
         (,%do-main (,s ,%whence ,returnv) ,@forms)))
    ))



(eval-when ()

  (setq *break-on-signals* t)

  (let ((iter (make-simple-vector-iterable
               '(1 2 3)
               :element-type #-NIL t #+NIL '(mod 4)))
        (call-reg))
;(macroexpand (quote
    (do-simple-vector-iterable (%elt iter (values call-reg t))
      (npushl (expt %elt 2) call-reg))
;    ))
    )

)



(defstruct (list-iterable
             (:include sequence-iterable)
             (:constructor make-list-iterable
                           (%members &key (element-type t)
                                     &aux (members (coerce %members 'list))))
             (:constructor))
  #+FIXME
  (members
   nil
   :type list))


#+TBD
(defmacro do-list-iterable ((s whence &optional returnv) &body forms)
  )
