;; macro-utils.lisp
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

(defmacro format* (ctrl &rest args)
  ;; NB: Defined here, rather than in seq-utils.lisp
  ;;     so as to avoid a certain dependency loop, as
  ;;     would occur if it was defined in that file.
  ;;
  "Return a simple string formatted with the format conrtol string CTRL
and the format arguments ARGS.

See also: `simplify-string'"
  `(format nil ,ctrl ,@args))


(defmacro with-gensym ((&rest names) &body body)
  `(let (,@(mapcar (lambda (name)
		     (let ((tmp (gensym (symbol-name name))))
		       `(,name (quote ,tmp))))
		   names))
     ,@body))

;; (macroexpand-1 '(with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4))))
;; (with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4)))

(defmacro with-symbols ((&rest names) &body body)
  ;; as an alternative to with-gensym
  ;;
  ;; FIXME Documentation. Note divergence onto the "with-gensyms" pattern
  `(let (,@(mapcar (lambda (name)
		     (let ((tmp (make-symbol (symbol-name name))))
		       `(,name (quote ,tmp))))
		   names))
     ,@body))

(defmacro intern* (s &optional (package *package*))
  "Intern a symbol with a name equivalent to the symbol S, interned in
the designated PACKAGE"
  `(intern (symbol-name ,s)
	   ,package))

;; (intern* (quote #:foo))


(defmacro intern-formatted (ctrl &rest args)
  ;; TD Define and apply a MAKE-SYMBOL-FORMATTED macro
  `(intern (format* ,ctrl ,@args)))

;; (intern-formatted "g_~a_foo" 'widget)


;; -- Macros onto the compiler environment

;; see also
;; ./common-opt.lisp
;; ./common-misc.lisp

(defmacro defconstant* (name value &optional docstring
                        &environment env)
  "Define NAME as a constant variable with value VALUE and optional
DOCSTRING

If NAME denotes an existing variable and its value is not EQUALP
to the specified VALUE, then a `SIMPLE-STYLE-WARNING' will be emitted,
and the previous value will be retained. This differs from the
behavior of `CL:DEFCONSTANT' in that EQUALP is applied as the
comparison, rather than EQL, moreover that the previous value will bex
retained in all instances of BOUNDP NAME.

If NAME does not denote an existing variable, then this macro's
behavior is analogous onto `CL:DEFCONSTANT'"
  (with-symbols (%value %previous)
    ;; FIXME arbitrary discard of return values from GET-SETF-EXPANSION

    ;; FIXME Needs source review [LTP]
    (let ((%name (nth-value 4  (get-setf-expansion name env))))
      ;; Ed. note: SBCL 1.2.5 was not handling a simpler form
      ;; when a certain file when McCLIM was compiled and loaded. So,
      ;; rather than using SYMBOL-VALUE directly on the NAME, this
      ;; will now try to wrap the reference to the symbol-value of
      ;; NAME around a return value from GET-SETF-EXPANSION w/ ENV
      ;; ...and still it doesn't work out. Effectively, it may be
      ;; that the BOUNDP call returns true (how?) but the SYMBOL-VALUE
      ;; call fails (how?) in "Some instances"
      ;;
      ;; So, EVAL instead of SYMBOL-VALUE ? Still, "Doesn't work out".
      ;; The 1st item in the backtrace is a FOP-FUNCALL, moreover,
      ;; as may serve to suggest a matter of some complexity towards
      ;; any complete debug of this particular issue.
      ;;
      ;; affected forms (McCLIM MCi fork)
      ;; * AUTOMATON::+MIN-CHAR-CODE+ (Drei state-and-transition.lisp)
      ;; * AUTOMATON::+INTERSECTION+ and later constants (Drei regexp.lisp)
      ;;
      ;; Each of those symbols is bound to a FIXNUM.
      ;;
      ;; Workaround: Use DEFCONSTANT instead, in those
      ;; bindings, considering: A FIXNUM is always EQL to itself
      ;;
      ;; Issue encountered in:
      ;;  * SBCL 1.2.5 Linux 64 bit
      ;;  * SBCL 1.2.5.76-65a44db Linux 64 bit
      `(defconstant ,name
         (cond
           ((boundp (quote ,%name))
            (let ((,%previous (symbol-value (quote ,%name)))
                  ;; ^ NB: LOAD-TIME-VALUE may DNW when binding %PREVIOUS
                  ;;
                  ;; NB This ensures, by side effect, that the VALUES
                  ;; expression is always evaluated in DEFCONSTANT*
                  (,%value ,value))
              (unless (equalp ,%value ,%previous)
                (simple-style-warning
                 "~<Ignoring non-EQUALP redefintion of constant ~S,~> ~
~<existing value ~S~> ~<ignored value ~S~>"
                 (quote ,%name) ,%previous ,%value))
              (values ,%previous)))
           (t ,value))
         ,@(when docstring
             (list docstring))))))


;; (defconstant* quux "foo")
;;
;; (defconstant* foo 'foo)
;; (defconstant* foo '"foo") ;; note style warning
;; (symbol-value 'foo)
;; => FOO
