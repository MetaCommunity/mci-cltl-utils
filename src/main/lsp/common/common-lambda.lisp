;; common-lambda.lisp
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

(defconstant* +declare-notype+
  ;; Untyped DECLARE specifiers
  ;;
  ;; These should be processed before any type declarations are parsed
  ;; from a DECLARE form
  '(dynamic-extent ignore optimize inline special ignorable notinline
    declaration))

#+NIL
(defconstant* +declare+
    ;; NB Not used in DEFUN*; provided for illustration
    (append '(type ftype) +declare-notype+))

(defconstant* +defun-lambda-param-kwd+
  '(&optional &rest &key &allow-other-keys))

(defconstant* +defun-lambda-kwd+
    (append +defun-lambda-param-kwd+
            '(&aux)))


(defmacro defun* (name lambda &rest forms &environment env)
  ;; TD: LABELS* - refer to remarks, below. "Test here"
  (macrolet ((pushl (v where)
                   `(setq ,where (nconc ,where (list ,v)))))
    (labels ((parse-docs (forms)
               (let ((frst (car forms)))
                 (cond
                   ((stringp frst)
                    (values (cdr forms) frst))
                   (t
                    (values forms nil)))))
             (parse-forms-declare (forms decls)
               ;; assumption: any docstring had been removed from forms
               ;;
               ;; NB: must handle multiple DECLARE
               (let ((frst (car forms)))
                 (cond
                   ((eq (car frst) 'declare)
                    (parse-forms-declare (cdr forms)
                                         (nconc decls
                                                (cdr frst))))
                   (t
                    (values forms decls)))))
             (parse-lambda-vars (llist)
               ;; NB: Return value for PARSE-DECL-TYPES LVARS (unused param)
               ;;     and PARSE-FTYPE LVARS
               (let (vars context)
                 (dolist (expr llist vars)
                   (cond
                     ((symbolp expr)
                      (cond
                        ((find expr +defun-lambda-kwd+ :test #'eq)
                         (setq context expr))
                        (t (pushl (cons expr context) vars))))
                     (t ;; assumption: EXPR is a CONS
                      (let ((vexpr (car expr)))
                        (etypecase vexpr
                          (symbol (pushl (cons vexpr context) vars))
                          ;; special &key forms:
                          (cons (pushl (cons (cadr vexpr) context) vars)))))
                     ))))

             (parse-decl-types (decls #+NIL lvars env)
               ;; TBD: Return value for PARSE-FTYPE DECLS
               (let (typed ;; ftyped cldecl classed other
                     vdecl
                     )
                 ;; NB: This does not need to provide a full, compiler-
                 ;; integrated defun (lambda) declarations parser. It
                 ;; must parse the declarations, to some extent, in order
                 ;; to determine which declarations denote types for
                 ;; bindings established per the defun lambda list.
                 ;;
                 ;; Note that some ambiguity is possible, in the normal
                 ;; declarations syntax. This parser is a best effort for
                 ;; addressing such ambiguities as may pertain to typing of
                 ;; lambda list variables.
                 (macrolet () ;; FIXME origin of pushl defmacro
                   (dolist (expr decls)
                     ;; sort each element of DECLS per declaration kind
                     (let ((kind (car expr)))
                       (cond
                         ((eq kind 'type)
                          (let ((type (cadr expr)))
                            (dolist (var (cddr expr))
                              (pushl (cons var type) typed))))
                         ((eq kind 'values)
                          ;; Assumption: CMUCL (or SBCL) lambda VALUES decl

                          ;; NB This does not warn about multiple VALUES
                          ;; decls - would implicitly use the last
                          ;; VALUES decl.

                          ;; FIXME prune this EXPR from the set of output
                          ;; declarations, when not either of CMUCL or
                          ;; SBCL - thus avoiding some style warnings.

                          ;; Note that this DEFUN* proposes to utilize
                          ;; the CMUCL (or SBCL) lambda VALUES declaration
                          ;; in producing a top-level FTYPE declaration -
                          ;; thus, supporting this convention of strong
                          ;; typing, insofar as in FTYPE declarations,
                          ;; for all implementations.
                          (setq vdecl expr))
                         ((eq kind 'ftype)
                          ;; NB a subset of TYPE decls
                          ;; Not per se used here (No-Op)
                          #+NIL (pushl expr ftyped)
                          )
                         ((find kind +declare-notype+ :test #'eq)
                          ;; No-Op
                          #+NIL
                          (pushl expr cldecl))
                         ((find-class kind nil env)
                          #+NIL (pushl expr classed)
                          #-NIL
                          (dolist (var (cdr expr))
                            (pushl (cons var kind) typed)
                            ))
                         (t ;; No-Op
                          #+NIL
                          (pushl expr other)))))
                   ;; NB: OTHER may contain non-class type-name and
                   ;; implementation-specific declarations.
                   ;;
                   ;; This revision of DEFUN* will not provide implementation-
                   ;; specific code.
                   ;;
                   ;; Assumption: Any declarations in OTHER will not provide
                   ;; type information about the LAMBDA list

                   ;; ...
                   (values typed vdecl)
                   )))
             (parse-ftype (name lvars type-map vdecl)
               (declare (type list type-map))
               ;; NB vis a vis VALUES in DECLARE w/ CMUCL and SBCL,
               ;; and CL:FTYPE declarations
               ;;
               ;; Parse any VALUES decls out of the DECLARE set, returning
               ;; this as a first value. Subsequently, use that value
               ;; (when non-nil) in a top-level FTYPE declaration - using
               ;; a default type T for any LAMBDA signature arguments that
               ;; have not been expressly typed in the DECLARE form
               ;;
               ;; When adapted for LABELS* the FTYPE declaration must be
               ;; handled for a declaration within the calling lexical
               ;; environment, a non-null lexical environment.

               ;; NB To retain portability, this will implement some
               ;; assumptions about type specifiers in the DECLARE list

               ;; NB: The FTYPE declaration must match not only the names
               ;; of variables provided in the lambda list - insofar as to
               ;; specify a type for each parameter. Furthermore, the
               ;; FTYPE declaration must match the grammar of the provided
               ;; lambda list.
               ;;
               ;; Note that this is not for typing of all lexical
               ;; variables that the lambda-list may specify to the
               ;; defining function. It pertains only to functional
               ;; parameters that would be visible within the calling
               ;; lexical environment.

               ;; FIXME still need to parse out the non-standard VALUES decls
               ;;
               ;; ... b.c stong typing in Common Lisp programs, juxtaposed
               ;; to a casual DECLARATION decl in non-CMUCL-fam lisps
               (let (param-spec context)
                 ;; Re-map LVARS x TYPE-MAP into a lambda-like PARAM-SPEC
                 (dolist (bkt lvars)
                   (destructuring-bind (var . ctxt) bkt
                     (unless (eq ctxt '&aux) ;; TBD: &AUX in FTYPE
                       (unless (eq ctxt context)
                         (setq context ctxt)
                         (pushl ctxt param-spec))
                       (let ((type-n (position var type-map
                                               :key #'car
                                               :test #'eq))
                             (type t))
                         (when type-n
                           (setq type (cdr (nth type-n type-map))))
                         (pushl type param-spec)
                       ))))
                 (values `(ftype (function ,param-spec ,vdecl) ,name)))
               )
             (parse-meta (name llist forms)
               (let ((lvars (parse-lambda-vars llist)))
                 (multiple-value-bind (forms docs)
                     (parse-docs forms)
                   (multiple-value-bind (forms decls)
                       (parse-forms-declare forms nil)
                     (multiple-value-bind (type-map vdecl)
                         ;; FIXME also return VDECL here,
                         ;; a VALUES declaration as per the CMUCL compiler
                         ;; i.e Python
                         (parse-decl-types decls #+NIL lvars env)
                       (multiple-value-bind (ftype)
                           (parse-ftype name lvars type-map vdecl)
                         (values ftype docs decls forms))))))
               ))

      (multiple-value-bind (ftype docs decls forms)
          (parse-meta name lambda (copy-list forms))
        `(progn
           (declaim ,ftype)
           (defun ,name ,lambda
             ,@(when docs (list docs))
             ,@(when decls (list `(declare ,@decls)))
             ,@forms)
           )))))


#+NIL
(eval-when ()

(macroexpand (quote
(defun* frob+ (m n o)
  "Trivially Typed +"
  (declare (fixnum m) (type integer n o)
           (values integer))
  (declare (dynamic-extent m n o))
  (+ m n o))
))
)

