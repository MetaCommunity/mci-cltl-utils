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



(in-package #:utils.ltp)

(defmacro format* (ctrl &rest args)
  "Return a simple string formatted of the format conrtol string CTRL
and the format arguments ARGS.

See also: `simplify-string'"
  `(format nil ,ctrl ,@args))


(defmacro with-gensym ((&rest names) &body body)
  `(let (,@(mapcar (lambda (name)
		     (let ((tmp (gensym (format* "%~A-" name))))
		       `(,name (quote ,tmp))))
		   names))
     ,@body))

;; (macroexpand-1 '(with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4))))
;; (with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4)))

(defmacro with-symbols ((&rest names) &body body)
  ;; as an alternative to with-gensym
  ;;
  ;; FIXME Documentation. Note divergence onto the common-ish "with-gensyms" pattern
  `(let (,@(mapcar (lambda (name)
		     (let ((tmp (make-symbol (format* "%~A-" name))))
		       `(,name (quote ,tmp))))
		   names))
     ,@body))

(defmacro intern* (s &optional (package *package*))
  "Intern a symbol with a name equivalent to the symbol S, interned in
  the designated PACKAGE"
  ;; FIXME =RENAME=> intern-alt
  `(intern (symbol-name ,s)
	   ,package))

;; (intern* (quote #:foo))


(defmacro intern-formatted (ctrl &rest args)
  ;; FIXME Define and apply a MAKE-SYMBOL-FORMATTED macro, where
  ;; apropos.
  ;;
  ;; FIXME Review this "Intern read-from-string" form, pursuant
  ;; towards documentation for this macro def.
  `(intern* (read-from-string (format* ,ctrl ,@args))))

;; (intern-formatted "#:g-~a-foo" 'widget)
		   
