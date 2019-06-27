;; defportable.lisp - Portable Signature Definition and Implementation in Lisp
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

#|

 Design Notes

 - cf. FEATUREP
 - cf. ./guarding.lisp [MT]

 Design State - Synopsis: Early prototype, APIs
   - Meta-Object Signature
   - Implementation Feature Set - cf. FEATUREP
   - Implementation Section, per
      (A) Source System, in Definition
      (B) Active Feature Set, in Evaluation
   - Meta-Object Implementation and Implementation-Signature Binding
     w/ Informative Feature Set storage

|#


(in-package #:ltp/common)

(defstruct (signature
             (:constructor))
  (name
   (mk-lf (make-symbol "Unnamed Signature"))
   :type sxhash-key
   :read-only t)
  (kind
   (mk-lf (make-symbol "Unspecified Kind"))
   :type symbol
   :read-only t)
  (docs
   ;; TBD: DOCS handling for FUNCTION-SIGNATURE, VARIABLE-SIGNATURE
   ;;
   ;; NB: DEFPORTABLE-VALIDATE is not a declarations iterator
   nil
   :type (or simple-string null)
   :read-only nil))


(defstruct (single-signature
             (:constructor)
             (:include signature))
  ;; A class for program meta-object signatures such that may be
  ;; provided, in each, with a single implementation
  ;;
  ;; NB: Juxtaposed to (TBD) CLASS-SIGNATURE
  ;;     such that may be provided with multiple implementations
  (impl ;; FIXME rename => implementation
   nil
   ;; NB may be EQ to NAME, portably, for any implemented variable-signature
   ;;
   ;; FIXME if this was implemented with a portable CLOS-like semantics,
   ;; (SLOT-BOUNDP THUNK 'IMPLEMENTATION) would serve in appliations,
   ;; alternate to storing NIL in this slot, for any unbound impl.
   ;;
   ;; In which case, :access (:read :write-once) may apply
   ;;
   :type (or function symbol) ;; or instance, condition, ...
   :read-only nil)
  (impl-feature-set
   ;; NB: To be provided by DEFIMPLEMENTATION
   nil
   :type (or symbol cons)
   :read-only nil))


(defstruct (function-signature
             (:include single-signature)
             ;; TBD: FTYPE declaration for FUNCTION-SIGNATURE
             (:constructor make-function-signature
                           (kind name args &optional arg-types values-type docs)))
  (name
   (mk-lf (make-symbol "Unnamed Function Signature"))
   :type (or (cons symbol t) symbol)
   :read-only t)
  (args
   nil
   :type list
   :read-only t)
  (arg-types
   nil
   :type list
   :read-only t)
  (values-type
   nil
   :type list
   :read-only t))


(defstruct (variable-signature
             (:include single-signature)
             ;; TBD: TYPE declaration for VARIABLE-SIGNATURE
             (:constructor make-variable-signature
                           (kind name &optional (type t) docs)))
  (name
   (mk-lf (make-symbol "Unnamed Variable"))
   :type symbol
   :read-only t))


#+TBD
(defstruct (class-template
             (:include signature))
  ;; ....

  ;; NB: May have multiple IMPLEMENTATION
  ;; as per a generalized class structure prototype

  ;; TBD: Analogy onto C++ template forms
  )

;; --

#+THISv1
(defun bind-impl (inst signature)
  ;; emit a specific style-warning if SIGNATURE already has a bound
  ;; IMPLEMENTATION not EQ to INST
  )


;; --


(defvar *avec-initial-length* 8) ;; NB: Mostly arbitrary default
(defmacro mk-avec (&key
                     (length nil lenp)
                     (fill-pointer nil fp)
                     (initial-contents nil cp))
  (with-symbols ( %contents %lenc %dim %fp)
    `(let* (,@(when cp `((,%contents ,initial-contents)))
            (,%lenc ,(when cp `(length ,%contents)))
            (,%dim ,(cond
                      (lenp length)
                      (cp %lenc)
                      (t (quote *avec-initial-length*))))
            (,%fp ,(cond
                    (fp fill-pointer)
                    (cp %lenc)
                    (t 0))))
       (make-array ,%dim
                   :fill-pointer ,%fp
                   :adjustable t
                   ,@(when cp
                       `(:initial-contents ,%contents))))))

;; (mk-avec :length 10)

;; (array-dimensions (mk-avec))
;; => (8)

;; (fill-pointer (mk-avec :fill-pointer 5))
;; => 5

;; (array-dimensions (mk-avec :fill-pointer 5))
;; => (8)

;; (fill-pointer (mk-avec :initial-contents '(1 2 3)))
;; => 3

;; (fill-pointer (mk-avec :fill-pointer 5 :initial-contents '(1 2 3)))
;;;; FIXME - the failure case is peculiar here w/ SBCL (1.4.16.debian)
;;;;         i.e warn about the fill-pointer, but error about the nr. args
;;;;
;;;; Well-formed expr, NB:
;; (macroexpand-1 (quote  (mk-avec :fill-pointer 5 :initial-contents '(1 2 3))))

;; (fill-pointer (mk-avec :fill-pointer 3 :initial-contents '(1 2 3)))
;; => 3

;; (fill-pointer (mk-avec :initial-contents '(1 2 3)))
;; => 3


(defmacro push-avec (elt whence)
  `(vector-push-extend ,elt ,whence *avec-initial-length*))


;; --
(defconstant +implementation+
  (let ((typ (lisp-implementation-type)))
    (multiple-value-bind (s alloc)
        (find-symbol typ (mk-lf (find-package :keyword)))
      (if alloc
          (values s)
          (error "~<Initialization not supported for +IMPLEMENTATION+~>~
~< on implementation ~S~>"
                 typ))))
  "Keyword symbol denoting this [lisp-implementation-type]
as per well-known *FEATURES* symbols")


;; TOPIC: System Structure & Reflection

(defstruct (impl-section
             ;; FIXME - abbreviated 'impl' prefix is inconsistent
             ;; per DEFIMPLEMENTATION
             (:constructor %mk-impl-section
                           (name &key
                                 (forms (mk-avec))
                                 (implementations (mk-avec))
                                 #+TBD_NESTED_FS (subsections (mk-avec))))
             (:constructor mk-impl-section
                           (name &optional (%forms nil %formsp)
                                 &aux (forms (cond
                                               (%formsp
                                                (mk-avec :initial-contents %forms))
                                               (t (mk-avec)))))))
  ;; TBD
  ;; - IMPL-SECTION source location, w/ support per
  ;;   - named filesystems - local to projects, site
  ;;   - truename for load file
  ;;   - load-file pathname for load file
  ;; - IMPL-SECTION onto LTP EO
  ;;   - IMPL-SECTION onto LLVM
  ;;   - IMPL-SECTION onto JLS - Static analysis (pre-JNI)
  (name
   (mk-lf (make-symbol "Unnamed Feature Section"))
   :type symbol
   :read-only t)

  (finalized-p
   :type boolean
   ;; NB: if FINALIED-P, the IMPLEMENTATIONS, FORMS, and non-nil SUBSECTIONS
   ;; contents should all be simple vectors for purpose of iteration
   :read-only nil)

  (forms
   (mk-avec)  ;; TBD: consider using EQ hash-tables onto (SXHASH NAME) (EQX-Table)
   :type (array t (*))
   :read-only nil)

  #+NIL
  (implementations
   ;; NB - bind the implementation to the signature form itself
   (mk-avec) ;; TBD: consider using EQ hash-tables onto (SXHASH NAME)
   :type (array t (*))
   :read-only nil)

  #+TBD_NESTED_FS
  (subsections
   ;; NB: A file represents a feature section - source locations
   nil
   ;; TBD: consider usihg an EQ hash-table onto NAME here, when non-nil
   :type (or null (array t (*)))
   :read-only nil)
  )

;; (make-impl-section (make-symbol "Unbound") '(1 2 3))
;; (make-impl-section (make-symbol "Unbound"))



#+TBD
(defun signature-declarations (...))

#+TBD
(defun signature-implemented-p (signature-spec whence-spec))
;; ^ may return a second value, the implementation (when defined to whence)


#+TBD
(declaim (type impl-section
               +null-impl-section+
               *section*))

#+TBD
(defconstant* +null-impl-section+
    (make-impl-section nil))

#+TBD
(defvar *section* +null-impl-section+)

#+TBD
(defun find-impl-section (name
                             #+TBD_NESTED_FS
                             &optional
                               #+TBD_NESTED_FS
                               (whence (find-feature-sectrion nil)))
  ;; TBD: impl-section nesting
  )


;; --

(defmacro define-decl-kind (kind (&rest config) &rest specification)
  ;; NB: Definition form for handlers for [TBD]
  ;; &PUNTING -- see examples below
  ;;   - DEFINE-DECL-KIND DEFUN
  ;;     - TBD (??) shared forms for
  ;;         DEFINE-DECL-KIND LTP/COMMON:DEFUN*
  ;;           - NB: DEFUN* might be ultimately disfavored as any manner
  ;;                 of a convenience form, pursuant of (A) normal,
  ;;                 portable FTYPE declrations at top-level, and
  ;;                 (B) signature forms as being developed in this system
  ;;         DEFINE-DECL-KIND DEFMACRO (w/ generic signature ??)
  ;;         DEFINE-DECL-KIND DEFGENERIC (??)
  ;;   - TBD DEFINE-DECL-KIND DEFVAR
  ;;     - TBD (??) shared forms for
  ;;         DEFINE-DECL-KIND DEFPARAMETER
  ;;         DEFINE-DECL-KIND DEFCONSTANT
  ;;         DEFINE-DECL-KIND LTP/COMMON:DEFCONSTANT*
  ;;
  )


(define-decl-kind defun ()
  (:parse-signature (name args &rest decls-docs)
    (let (docs type-decls values-decl
               parser-context
               ftype-types)
      (labels ((duplicate-err (kind init subsq)
                 (error "~<Duplicate ~A for ~S ~
defun signature.~>~< Initial: ~S>~< Second: ~S~>"
                        kind name init subsq))
               (unrecognized-err (spec)
                 (error "~<Unrecognized ~S parameter: ~S~>"
                        context spec))
               (add-type-decl (decl)
                 (destructuring-bind (kwd type &rest args) decl
                   (declare (ignore kwd))
                   (dolist (a args)
                     ;; FIXME - does not check for duplicates, here
                     (push (cons a type) type-decls))))
               (find-type-decl (name)
                 (assoc name type-decls :test #'eq))
               (add-ftype-elt (elt)
                 (npushl elt ftype-types))
               (find-keyword-parm (spec)
                 (let ((first (car spec)))
                   (etypecase first
                     (symbol first)
                     (cons (cadr first)))))
               (find-keyword-key (spec)
                 (let ((first (car spec)))
                   (etypecase first
                     (symbol (intern (symbol-name first) a
                                     (mk-lf (find-package :keyword))))
                     (cons (car first))))))
        (dolist (elt decls-docs)
          (etypecase elt
            (string
             (cond
               (docs
                (duplicate-err "documentation strings" docs elt))
               (t (setq docs elt))))
            (cons
             (case (car elt)
               (type (add-type-decl elt))
               (values
                (cond
                  (values-decl
                   (duplicate-err "values declarations" values elt))
                  (t (setq values-decl elt))
                  ))))))
        ;; iterate across ARGS to match type decls for FTYPE decl
        (dolist (elt args)
          (etypecase elt
            (symbol
             (cond
               ((position (the symbol elt) (the cons lambda-list-keywords)
                          :test #'eq)
                (setq parser-context elt)
                (add-ftype-elt elt))
               (t (let ((typ (find-typ-decl elt)))
                    (add-ftype-elt (if typ (cdr typ) t))))))
            (cons
             ;; assumption: ELT represents an &OPTIONAL, &KEY, or &AUX parameter
             ;;
             ;; NB: Convenience for &KEY type decls in signtaures
             (case context
               (&optional
                (let ((typ (find-type-decl (car elt))))
                  (add-ftype-elt (if typ (cdr typ) t))))
               (&key
                (let* ((param (find-keyword-param elt))
                       (kwd (find-keyword-key elt))
                       (typ (find-type-decl param)))
                  (add-ftype-elt (list kwd (if typ (cdr typ) t)))))
               (&aux
                (let ((typ (find-type-decl (car elt))))
                  (add-type-elt (if typ (cdr typ) t))))
               (t (unrecognized-err elt))))))
        (unless values-decl
          (setq values-decl (mk-lf (quote (values t &optional)))))
        ;; ... and define - see TBD_TEST for DEFSIGNATURE, below
        (make-function-signature 'defun name args
                                 ftype-types values-decl
                                 docs))))

  #+TBD
  (:unparse-declarations ...)

  ;; NB: Still need to "Wire" each implementation signature,
  ;; feature-set, and implementation form together for each signature in
  ;; each context
  ;; - cf. SIGNATURE-IMPL-FEATURE-SET
  ;;   and also DEFIMPLEMENTATION
  (:parse-implementation (name args &rest forms)
                         (declare (ignore args forms))
                         ;; &PUNTING - return a value suitable as an SXHASH key
                         (cons 'defun name)))

#+TBD
(define-decl-kind defvar ()
  (:parse-signature (name &rest decls-docs)
   )
  (:parse-implementation (name &optional (init init-p) docs)
                         ))


#+TBD
(defun parse-declaration-signature (form &optional (context *section*))
  ;; TBD: May be reuesd in DEFSIGNATURE and DEFIMPLEMENTATION
  (labels ((parse-decl-docs (form)
             )
           (add-decl (name &rest info) ;; per *SECTION*
             ))
  (destructuring-bind (kind name &rest info) form
    (ecase kind
      ((defun defun*)
       (make-function-signature kind name ...parse-info...)
       )
      ((defvar defconstant defconstant* defparameter)
       (make-variable-signature kind name ...parse-info...)
       )
      ;; ...other-handler... in CONTEXT
      ))))



#+THISv1 ;; syntax TBD
(defmacro declare-section (section-name form))
;; ^ declare a build-time section by section name .. TBD FORM

#+THISv1
(defmacro define-feature-set (name expr))
;; ^ declare a build-time feature-set
;;
;; - TBD impl-section+ forms to a feature-set, for purpose of sytems review



(defmacro defsignature ((&optional (context *context))
                               expr)
  )
;; ^ declare a build-time signature ...


#+TBD
(defmacro signature-declaim (signature &optional (whence *context*))
  )


#+TBD_TEST
(defsignature ()
    (defun frob-signature (a &key ((:b %b) nil) (c nil c-p)
                               &rest %other &aux (other (when %other (coerce %other 'simple-vector))))
      (values t &optional)
      (type fixnum a %b c)
      (type list %other)
      (type (or simple-vector null) other)
      "frob-signature function description"))


(defmacro defimplementation ((when-features &optional (context *context*))
                                        expr)

  ;; ... binding the result of the evaluation of EXPR to its parsed EXPR name,
  ;;     under *CONTEXT* .. during compile??
  ;;     ... Otherwise only returning the result of the evaluation of EXPR?
  )

#+TBD_TEST
(defsignature (#.+implementation+)
    (defun frob-signature (a &key ((:b %b) 0) (c 0 c-p)
                               &rest %other &aux (other (when %other (coerce %other 'simple-vector))))
      (list (+ a (/ %b c)) other)))

#+TBD
(defun clear-impl-section-table ())
;; ^ begining at +null-impl-section+, conduct a depth-first traversal
;;   to DEALLOCATE-IMPL-SECTION, thus freeing each decl, impl object
;;   for purpose of GC


#+TBD_TESTS
(setq *context* (define-feature-set +thunk-1+))

#+TBD_TESTS
(defsignature ()
  (defconstant +global-thunk+))

#+TBD_TESTS
(defsignature ()
  (defconstant +global-unthunk+))


#+TBD_TESTS
(defimplementation ((:context #.(feature-set (quote +thunk-1+)))
              (:case (get-impl-symbol)))
  (defconstant* +global-thunk+ "Some well-known impl-specific symbol"))

#+TBD_TESTS
(deportable-validate :context '+thunk-1 :case (get-impl-symbol)
                     :behavior error)

;; ...

#+THISv1
(defmacro fs (name))
;; ^ dereference a build-time feature-set by name

#+TBD
(defun parse-definition-signature (form &optional (context *section*)))

#+THISv1
(defmacro defportable-define (section feature-set eval-form))
;; ^ evaluate EVAL-FORMS as a build-time section IFF FEATURE-SET
;;   denotes a "True" feature set

#+TBD
(defmacro defportable-validate (section &optional (policy :warn)))
;; ^ emit any warnings/errors - per TBD policy - for any elements of the
;;   named section that are not defined in the implementation.
