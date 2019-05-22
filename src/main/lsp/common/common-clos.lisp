;; clos-utils.lisp
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


(defmacro call-next-method* (&rest args)
  (with-symbols (%nmp)
    `(let ((,%nmp (next-method-p)))
       (cond
	 (,%nmp (call-next-method ,@args))
	 (t (values nil nil))))))

(defmacro slot-value* (object slot &optional default)
  ;; NOTE: This is defined as a macro, so as to allow the compiler to
  ;; optimize the SLOT-VALUE call, if applicable
  (with-symbols (o s)
    `(let ((,o ,object)
           (,s ,slot))
       (cond
         ((slot-boundp ,o ,s)
          (values (slot-value ,o ,s)
                  t))
         (t (values nil ,default))))))



(defmacro when-slot-init ((instance name sl-names-var) &body body)

  "WHEN-SLOT-INIT provides a convenience macro for initialization of
slot values with effective initial forms, within SHARED-INITIALIZE.

The INSTANCE expression should evaluate to an object provided to
SHARED-INITIALIZE.

The NAME expression should denote a symbol naming the slot to be
initialized. This expression will not be evaluated.

The SL-NAMES-VAR expression should evalute to the object provided as the
second argument to SHARED-INITIALIZE


The BODY form will be evaluated under the intersection of the
following conditions:

 1) when SL-NAMES-VAR effectively indicates that the initial form should
    be evaluted for the slot NAME[1] within SHARED-INITIALIZE.

 2) when the slot NAME is not bound in the INSTANCE

Example use case:
   Slot A-FUNCTION with its value dervied from slot A-FORM as a
   function compiled in the lexical environment of a method
   specialized onto SHARED-INITIALIZE

 (defclass %eval ()
   ((a-form :initarg :a :accessor %eval-a-form :type list)
    (a-function :accessor %eval-a-function :type function)))

 (defmethod shared-initialize :after ((instance %eval)
                                      sl-names-var
                                      &rest initargs
                                      &key &allow-other-keys)

   (when-slot-init (instance a-function sl-names-var)
      (setf (%eval-a-function instance)
            (compile nil (%eval-a-form instance)))))


 (let ((inst (make-instance '%eval :a '(lambda () (+ 2 2)))))
   (funcall (%eval-a-function inst)))

 => 4

Footnotes
 [1] i.e when SL-NAMES-VAR is T or when SL-NAMES-VAR is a CONS and NAME is
     an element of SL-NAMES-VAR
"
  (with-symbols (%inst %name %sl-names-var)
    `(let ((,%inst ,instance)
           (,%name (quote ,name))
           (,%sl-names-var ,sl-names-var))
       (when (and (or (eq ,%sl-names-var t)
                      (and (consp ,%sl-names-var)
                           (find ,%name (the cons ,%sl-names-var)
                                 :test #'eq)))
                  (not (slot-boundp ,%inst ,%name)))
         ,@body))))

