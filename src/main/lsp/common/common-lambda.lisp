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
    ;; NB: This provides a more specialized set of symbols than
    ;; CL:LAMBDA-LIST-KEYWORDS
  '(&optional &rest &key &allow-other-keys))

(defconstant* +defun-lambda-kwd+
    (append +defun-lambda-param-kwd+
            '(&aux)))

(defvar *default-ftype-values* '(values t))

;; FIXME update the FTYPE routines for specialized &KEY args

(defmacro defun* (name lambda &rest forms &environment env)
  ;; TD: LABELS* - refer to remarks, below. "Test here"
  ;; - via PARSE-NAMED-LAMBDA - refer to function signature, below
  (labels ((parse-docs (forms)
             (let ((frst (car forms)))
               (cond
                 ((stringp frst)
                  (values (cdr forms) frst))
                 (t
                  (values forms nil)))))
           (parse-forms-declare (forms decls)
             ;; assumption: any docstring has been removed from forms
             ;;
             ;; NB: Handle multiple DECLARE
             (let ((frst (car forms)))
               (cond
                 ((eq (car frst) 'declare)
                  (parse-forms-declare (cdr forms)
                                       (nconc decls
                                              (cdr frst))))
                 (t
                  (values forms decls)))))
           (parse-lambda-params (llist)
             ;; NB: This function's return value is used together with
             ;; the return value from PARSE-DECL-TYPES, for computing
             ;; an FTYPE declaration in PARSE-FTYPE
             ;;
             ;; This function, as such, provides a value for the
             ;; LPARMS parameter to PARSE-FTYPE
             ;;
             ;; Usage - Within PARSE-FTYPE, this function's return
             ;; value is processed within an iterative form, such that
             ;; will compute a type for each lambda parameter parsed
             ;; out by PARSE-LAMBDA-PARAMS. For any parameter not
             ;; given an explicit type in any declaration forms,
             ;; PARSE-FTYPE will provide a resonable default type -
             ;; similarly, for the VALUES element in the resulting
             ;; FTYPE declaration.
             ;;
             ;; For &key arguments, these two functions will utilize a
             ;; specific, list-based data structure:
             ;;   (CONTEXT . (VAR . KEYNAME))
             ;; juxtaposed to the data structure used for other lambda
             ;; list parameters:
             ;;   (CONTEXT . VAR)
             (let ((kwdpkg (find-package '#:keyword))
                   vars context)
               (declare (type package kwdpkg)
                        (dynamic-extent kwdpkg))
               (dolist (expr llist vars)
                 (etypecase expr
                   (symbol
                    (cond
                      ((find expr +defun-lambda-kwd+ :test #'eq)
                       (when (eq expr (quote &allow-other-keys))
                         ;; NB: This does not match a VARINFO
                         ;; bucket - using NIL as a placeholder for a
                         ;; parameter name. In usage, the CDR of this
                         ;; should ultimately be ignored - as for the
                         ;; &allow-other-keys context
                         (npushl (cons expr nil) vars))
                       (setq context expr))
                      ;; NB: All symbol type expressions not denoted
                      ;; in +DEFUN-LAMBDA-KWD+ should be parsed subsq.
                      ((eq context (quote &key))
                       ;; NB: already parsed the &KEY keyword
                       (let ((key (intern (symbol-name expr)
                                          kwdpkg)))
                         (npushl (cons context (cons expr key))
                                 vars)))
                      (t (npushl (cons context expr) vars))))
                   (cons
                    ;; assumptions: EXPR is a CONS - thus decribing an
                    ;; &optional or &key parameter. These specifiers
                    ;; will include further information than a
                    ;; parameter name, of course.
                    (let ((vexpr (car expr)))
                      (case context
                        (&aux) ;; no-op
                        (&optional
                         (npushl (cons context vexpr) vars))
                        (&key
                         (etypecase vexpr
                           (symbol
                            (let ((key (intern (symbol-name vexpr)
                                               kwdpkg)))
                              (npushl (cons context (cons vexpr key))
                                      vars)))
                           (cons
                            ;; Specialized &key forms
                            (let ((key (car vexpr))
                                  (var (cadr vexpr)))
                              (npushl (cons context (cons var key))
                                      vars)))))
                        ;; FIXME: This style warning introduces NAME
                        ;; and LAMBDA from the calling lexical
                        ;; environment. As such, it introduces a
                        ;; concern with regards to porting this lambda
                        ;; list parser for both LABELS* and DEFUN*
                        ;;
                        ;; Note also, BOA lambda list forms in
                        ;; constructor specifications for DEFSTRUCT -
                        ;; vis a vis, "Ways to define a class" in CLOS
                        ;; and MOP systems.
                        (t (simple-style-warning
                            ;; NB: Only reached for CONS type lamba
                            ;; element exprs
                            "~<Ignoring lambda list expression ~S ~S~>~
~< in DEFUN* ~S ~S~>" context expr name lambda)))
                      ))))))

           (parse-decl-types (decls env)
             ;; TBD: Return value for PARSE-FTYPE DECLS
             (let (typed
                     ;;; unused lambda parser parameters
                   ;; ftyped cldecl classed other
                   vdecl)

               ;; NB: This does not need to provide a full, compiler-
               ;; integrated defun (lambda) declarations parser. It
               ;; must parse the declarations, to some extent, in
               ;; order to determine which declarations denote types
               ;; for bindings established per the defun lambda list.
               ;;
               ;; Subsequent functions may use this function's return
               ;; value, for estalblishing type declarations for
               ;; function parameters, such that would be visible from
               ;; any calling lexical environment.
               ;;
               ;; Note that some ambiguity is possible, in the normal
               ;; declarations syntax. This parser is a best effort for
               ;; addressing such ambiguities as may pertain to typing of
               ;; lambda list parameters.
               (dolist (expr decls)
                 ;; sort each element of DECLS per declaration kind
                 (let ((kind (car expr)))
                   (cond
                     ((eq kind 'type)
                      (let ((type (cadr expr)))
                        ;; record the type for each explicitly typed
                        ;; variable
                        (dolist (var (cddr expr))
                          (npushl (cons var type) typed))))
                     ((eq kind 'values)
                      ;; Assumption: This EXPR denotes a CMUCL lambda
                      ;; VALUES declaration. This declaration may also
                      ;; be evaluated in SBCL.

                      ;; NB This does not warn about multiple VALUES
                      ;; declarations - would implicitly use the last
                      ;; VALUES decl.

                      ;; FIXME prune this VALUES EXPR from the set of
                      ;; output declarations, when not either of CMUCL
                      ;; or SBCL - thus avoiding some style warnings,
                      ;; in implementations assumed not to implement
                      ;; the VALUES declaration, per se.

                      ;; Note that this DEFUN* proposes to utilize
                      ;; the CMUCL (or SBCL) lambda VALUES declaration
                      ;; in producing a top-level FTYPE declaration -
                      ;; thus, supporting such a convention of strong
                      ;; typing, insofar as in FTYPE declarations,
                      ;; for all implementations.
                      ;;
                      ;; Implementation-specific optimizations would
                      ;; be beyond the scope of this comment
                      (setq vdecl expr))
                     ((eq kind 'ftype)
                      ;; NB a manner of a subset of TYPE decls
                      ;; Not per se used here (No-Op)
                      #+NIL (npushl expr ftyped)
                      )
                     ((find kind +declare-notype+ :test #'eq)
                      ;; No-Op
                      #+NIL
                      (npushl expr cldecl))
                     ((find-class kind nil env)
                      #+NIL (npushl expr classed)
                      #-NIL
                      (dolist (var (cdr expr))
                        (npushl (cons var kind) typed)
                        ))
                     (t ;; No-Op
                      #+NIL
                      (npushl expr other)))))
               ;; NB: OTHER may contain non-class type-name and
               ;; implementation-specific declarations
               ;;
               ;; FIXME: This, in its present revision, will not
               ;; recognize any shorthand type declarations that do
               ;; not represent class names.
               ;;
               ;; This revision of DEFUN* will not provide implementation-
               ;; specific code.
               ;;
               ;; Assumption: Any declarations in OTHER will not provide
               ;; type information about the LAMBDA list

               ;; ...
               (values typed vdecl)))
           (parse-ftype (name lparms type-map vdecl)
             (declare (type list type-map))
             ;; NB vis a vis VALUES in DECLARE w/ CMUCL and SBCL,
             ;; and CL:FTYPE declarations
             ;;
             ;; Parse any VALUES decls from the original DECLARE set.
             ;; Subsequently, use that value (when non-nil) in a
             ;; top-level FTYPE declaration - using a default type T
             ;; for the VALUES type and for any LAMBDA parameters
             ;; that have not been expressly typed in the DECLARE form.
             ;;
             ;; When adapted for LABELS* the FTYPE declaration must be
             ;; handled for a declaration within the calling lexical
             ;; environment, a non-null lexical environment -- vis a vis:
             ;;  PARSE-NAMED-LAMBDA (NAME LAMBDA FORMS KIND ENV) => FTYPE, DOCS, FORMS
             ;;
             ;; NB: PARSE-NAMED-LAMBDA INLINE

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
               ;; Re-map LPARMS x TYPE-MAP into a lambda-like PARAM-SPEC
               (dolist (bkt lparms)
                 ;; NB: for any &key parameter, the CDR of BKT will be
                 ;; a list -- as produced within PARSE-LAMBDA-PARAMS --
                 ;; to a general format: (CONTEXT . (VAR . KEYNAME))
                 ;;
                 ;; For any other lambda list parameter, the CDR of
                 ;; BKT should be a symbol, denoting a variable name
                 ;; within the specified lambda list context, CTXT
                 (destructuring-bind (ctxt . varinfo ) bkt
                   (case ctxt
                     (&aux) ;; no-op
                     (&allow-other-keys
                      (npushl ctxt param-spec))
                     (t
                      (unless (eq ctxt context)
                        (setq context ctxt)
                        (npushl ctxt param-spec))
                      (let* ((varname (etypecase varinfo
                                        (cons (car varinfo))
                                        (symbol varinfo)))
                             (type-n (position varname type-map
                                               :key #'car
                                               :test #'eq))
                             ;; FIXME: Parameterize this default type
                             (type t))
                        (when type-n
                          ;; else, initial type 'T' is used
                          (setq type (cdr (nth type-n type-map))))
                        (case ctxt
                          ;; FIXME use a default type LIST for any
                          ;; &REST param
                          (&key
                           (let ((kwd (cdr varinfo)))
                             ;; Note the remark about "Specialized &key
                             ;; forms" in PARSE-LAMBDA-PARAMS local defun
                             (npushl (list kwd type) param-spec)))
                          (t
                           (npushl type param-spec))))
                      )))) ;; DOLIST
               ;; Provide a default value for VDECL
               (unless vdecl
                 (setq vdecl *default-ftype-values*))
               (values `(ftype (function ,param-spec ,vdecl) ,name))))

           (parse-meta (name llist forms)
             (let ((lparms (parse-lambda-params llist)))
               (multiple-value-bind (forms docs)
                   (parse-docs forms)
                 (multiple-value-bind (forms decls)
                     (parse-forms-declare forms nil)
                   (multiple-value-bind (type-map vdecl)
                       (parse-decl-types decls env)
                     (multiple-value-bind (ftype)
                         (parse-ftype name lparms type-map vdecl)
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
         ))))


#+NIL
(eval-when ()

(macroexpand (quote
(defun* frob+ (m n o)
  "Trivially Typed +"
  (declare (fixnum m) (type integer n o)
           (values integer &optional))
  (declare (dynamic-extent m n o))
  (+ m n o))
))

(frob+ 1 2 3)

(defun* frob* (m n &optional (o 1))
  "Trivially Typed *"
  (declare (fixnum m)
           (values integer &optional))
  (declare (type integer o)
           (dynamic-extent m n o))
  (* m n o))

(frob* 1 1)

(frob* 1 1 2)


(defun* frob- (m n &optional (o 1))
  "Trivially Typed -"
  (declare (fixnum m))
  (declare (dynamic-extent m n o)
           (type integer o))
  (- m n o))



(macroexpand (quote
(defun* find-frob (digit where &rest kwlist
                         &key ((:start %start) 0) from-end
                         &allow-other-keys &aux (c (digit-char digit)))
  "Find NIL"
  (declare (type (mod 10) digit) (string where)
           (ignore kwlist)
           (type (mod #.array-dimension-limit) %start))
  (let ((n
         (position c where :test #'char= :start %start
                   :from-end from-end)))
    (declare (type base-char c))
    (cond
      (n (values c n))
      (t (values nil nil )))))
))



;; (find-frob 1 "54321012345")
;; (find-frob 1 "54321012345" :from-end t)

)

