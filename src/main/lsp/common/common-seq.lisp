;; common-seq.lisp - utilities generalized for Common Lisp sequnces
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

(defmacro do-map ((sym whence &optional return-type)
                     &body forms)
  "Evaluate FORMS for each element of sequence WHENCE bound to symbol SYM

Similar to DOLIST and other such standard iterative macros, the FORMS
expression will be iteratively evaluated in a lexical environment within
a block named NIL. As such, the FORMS may return with RETURN-FROM, as in
order to exit from the iterative application of FORMS, before evaluation
on all seqeuence elements of WHENCE.

The optional RETURN-TYPE, a static symbol denoting the type of the value
to return from the corresponding MAP call, will not be evaluated.

This macro provides an iterative procedure syntactically similar to
DOLIST. By comparison, DO-MAP may be applied onto arbitrary sequence
types. Moreover, DO-MAP provides an optional storage for the final
return value from each successive evaluation of FORMS, such that the
optional storage sequence will provide the return value from the
evaluation of DO-MAP. If RETURN-TYPE is the symbol NIL, the return value
from each evaluation of FORMS is assumed to be discarded by the
implementation, as within the underlying MAP call in this macro's
macroexpanded form.

Like as with DOLIST, FORMS may be prefixed with any number of DECLARE
specifications, such that should be interpreted as local to the
function enclosing the FORMS.

See also: DO-MAPPED such that provides an alternate return value
semantics for iterative evaluation onto DO-MAP."
  (with-symbols (fn)
    `(let ((,fn (lambda (,sym) ,@forms)))
       (block nil
         ;; NB: If ever using MAP-INTO NIL, below, the implementation
         ;; may opportunistically skip the eval.
         ;;
         ;; When using MAP NIL, instead, the forms are - in no less -
         ;; evaluated normally. The return value may not be stored,
         ;; however, for each successive evaluation of the forms onto
         ;; the sequence construed from evaluation of WHENCE, for
         ;; the call to MAP NIL
         (map (quote ,return-type) (the function ,fn) ,whence)))))

;; (do-map (c "Frob") (print (the character c) *debug-io*))

;; --

(defmacro do-mapped ((sym whence &optional (retv nil retvp))
                     &body forms)
  ;; NB: This, in effect, deprecates DO-VECTOR
  "Evaluate FORMS for each element of sequence WHENCE bound to symbol SYM

Similar to DOLIST and other such standard iterative macros, the FORMS
expression will be iteratively evaluated in a lexical environment within
a block named NIL. As such, the FORMS may return with RETURN-FROM, as in
order to exit from the iterative application of FORMS, before evaluation
on all seqeuence elements of WHENCE.

If a return value expression is provided as RETV, the macroexpanded form
will return with that return value expression, after normal exit from
the iterative evaluation. If no such return value expression is
provided, the macroexpanded form will return a nil values set. If the
evaluation of FORMS returns to the block NIL for any element of WHENCE,
any return value expression - if provided - will be evaluated subsequent
of that return call. Alternately, DO-MAPPED may be evaluated in an
environment providing a block other than NIL, such that any evaluation
of FORMS may use as the RETURN-FROM block name.

This macro provides an iterative procedure syntactically similar to
DOLIST. By comparison, DO-MAPPED is implemented with MAP NIL. Thus,
it can be used for iteration on vector values or lists.

Like as with DOLIST, FORMS may be prefixed with any number of DECLARE
specifications, such that should be interpreted as local to the
function enclosing the FORMS.

This macro is implemented onto DO-MAP."
  `(progn
     ;; NB, Assumption: DO-MAP expands to evaluation in BLOCK NIL
     (do-map (,sym ,whence nil)
       ,@forms)
     ,(if retvp
          retv
          '(values))))


;; (do-mapped (c "Frob" 'frob) (print (the character c) *debug-io*))
;; (do-mapped (c "Frob") (print (the character c) *debug-io*))
