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

#|

  Topic: `DEFUN*` Redesign

  NB: Redesign finalized in the LTP-Main source repository, at changeset
  [55cd4cd79d87d3cb66adca25a2217252c38261de] 2019-06-29 12:04:20+0000

  The following documentation may refer to source forms in changesets
  previous to that finalization.


  The DEFUN* macro was originally designed as to provide a portable
  emulation _vis a vis_ `VALUES` declarations in `DEFUN` -- the latter,
  as typically supported in CMUCL and SBCL Common Lisp implementations.

  Pursuant of the definition of the LAMBDA* macro, the definition of the
  LAMBDA-SINGATURE system in the LTP-Main source repository, the
  general goal as to support a semantics similar to DEFUN* for LABELS
  and DEFMACRO, and -- in applications of the LAMBDA* macro -- the goal
  of supporting localiczed FTYPE declarations, such as for extensions
  onto CLOS and MOP, DEFUN* is being generally redesigned from its
  original definition.

  In the original definition of DEFUN* in the LTP-Main source
  repository, the macro was defined as to use a small number of
  lexically scoped functions, for a general purpose of:

  1. Parsing the provided lambda list, to detect parameters to the
     corresponding DEFUN lambda form

  2. Parsing any declarations provided to the DEFUN* form, _vis a vis_
     conventional DECLARE semantics in DEFUN, extended with the
     addition of the VALUES declaration.

  3. Parsing out any documentation string provided, for the expansion to
     DEFUN in the macro form.

  4. Producing an FTYPE declaration, assembled of any provided type
     declarations -- using the default type, T -- and any provided
     VALUES declaration -- using the default VALUES type, (VALUES T) --
     from declarations provided within the DEFUN* forms expression.

  5. Producing the corresponding FTYPE declaration and the intrinsic
     DEFUN form for evaluation, in the macroexpansion.

  It is expected that this generalized application pattern will have
  been retained, across the DEFUN* redesign.

  While DEFUN* may provide a manner of convenience, for definitions of
  strongly typed, named functions in the null lexical environment, the
  implementation of DEFUN* as a macro form has not been without some
  discrete concerns.

  - It should be portable, in its implementation, for supporting a
    similar semantics for definition of a by-in-large strongly
    typing "Wrapper Macro" for LABELS forms and other forms generally
    operating in function objects.

  - A portable implementation of DEFUN* should be reusable for an
    implementation of a methodology for strong typing of arbitrary
    anononymous lambda functions, _vis a vis_ the LAMBDA* macro as
    defined in this source system. It may be assumed that an FTYPE
    declaration may be usable for forms similar to
      (funcall (the <FTYPE_FUNCTYPE> <FN>) <ARGS...>)
    in some Common Lisp implementations.

  - A portable definition of DEFUN* may furthermore be reusable for
    definitions of generalized operations on lambda signature forms for
    accessors and funcallable standard objects, _vis a vis_ extensions
    onto CLOS and MOP.

  In its original implementation, DEFUN* produced a small number of
  ephemeral CONS objects, namely as pursuant of the construction of an
  effective, declared types table for lambda list parameters, and
  otherwise as for storage of lambda list parameter names, pursuant to
  producing an FTYPE form of the lambda list provided to DEFUN*. It was
  subsequently redesigned to use, in a manner, _callback functions_
  with discrete function signatures, in lieu of arbitrary list
  construction in the parser functions, themselves.


  During the redesign of the the DEFUN* implementation, a generalized
  approach was developed as follows:

  1. Move and optionally rename the set of lexically scoped functions,
  such as to be defined at the lexical _top level_, in the defining
  source file.

  2. Redefine those functions such as to "Fold" any effective top-level
  functionality into a minimum of top-level functions. During this
  effort, in the redesign, the _callback_ and _type hash table_
  additions, denoted above, may be introduced to the DEFUN*
  implementation. The latter addition may, in effect, be developed as in
  a manner extensional to the updated, parser-callback approach.

  3. Expand the test set of the original DEFUN* such as to ensure a
  consistent handling for functions using arbitrary keyword arguments
  for lambda-list parameters, namely such as for any keyword argument
  differing in symbol name, between the effective  _call form_ and
  _lambda parameter form_ for the respective keyword argument form.

  4. Test by extension, in adopting the updated implementation for
  support of:

      - Emitting an FTYPE declaration as a return value in evaluation of
        the macroexpansion for LAMBDA*. This may be tested by extension,
        subsequently, for a "DEFCLASS like DEFSTRUCT" extension onto
        CLOS and MOP, in which the emitted FTYPE declaration -- such as
        when constructing any single slot value initform's lambda
        function -- may be used in a manner principally internal to the
        extension, as such. This may follow the general pattern, in
        application, `(funcall (the <FTYPE> <FN>) <ARGS...>)`

      - Adding FTYPE declarations for lexically scoped functions defined
        in a LABELS context - assumimg that the added FTYPES
        declarations may be of use as information to the compiler, in
        some Common Lisp implememtations. This, in effect, may represent
        an effort for supporting localized VALUES declarations in
        lexically scoped LABELS functions, in Common Lisp implementations
        beside CMUCL or SBCL.

      - Implementation-specific forms emulating `(COERCE <FORM> 'FUNCTION)`
        within any expressly provided lexical environment not the _null
        lexical environment_. This may be approached, in an implementation-
        specific manner, broadly as complimentary to the definition of
        the updated `LAMBDA*` macro. This matter, although not described
        above, may be developed as for concerns with regards to
        lexically scoped symbols -- as typed or untyped variables --
        such that may be accessed for their lexically scoped symbol
        values, during compile time, and lexically scoped condition
        handlers within any non-null lexical environment, such as
        principally when compiling an anonymous lambda function.
        Perhaps there may be any concern discovered with regards to
        lexical scoping for _restart forms_ in non-null lexical
        environments, moreover. While not singularly dependent on any
        methodology in a definition for DEFUN*, but as this concern may
        be approached in a manner effectively complimentary to an
        update to the definition of the portable LAMBDA* macro, it
        therefore finds a concern in relation to the update to the
        definition of DEFUN*.

      - Formal modeling for lambda list forms in LAMBDA-SIGNATURE
        objects. This is presently being developed, within other source
        forms in the LTP-Main source repository.

  Subsequently, these updated may be adopted -- generally, in
  application and for  purposes of reflective source system modeling --
  in the CommonIDE project, an informal initiative of Thinkum Labs.

|#


;; --------------------

;; NB: UNPARSE-FTYPE-FUNCTION-DECLARATION (lambda-l forms &optional eval-context)

;; TBD; UNPARSE-FTYPE-NAMED-DECLS (name lambda-l decls &optional eval-context)
;; ^ for defportable.lisp DEFSIGNATURE

;; ^ NB EVAL-CONTEXT: A symbol (or cons of symbol), uniquely denoting the context in which
;; the function may be understood as being evaluated, e.g DEFUN*
;; for when any error condition is detected during FTYPE unparsing for a
;; DEFUN* macroexpansion. May be printed readably for notation in the
;; corresponding condition report stream, on event of error. For purpose
;; of editor integration, should be - as a symbol - stored in the
;; initialized CONDITION object representing the error.

(deftype eval-context ()
  '(or symbol list string standard-object structure-object))

(define-condition macroexpansion-condition ()
  ;; FIXME : Move to COMMON-CONDITION.LISP
  ;;
  ;; Note extensions/applications in system reference documentation.
  ((context
    ;; NB: symbol or cons of generally symbolic designator, denoting the
    ;; context of evaluation in which the MACROEXPANSION-CONDITION has
    ;; occurred - for assistance in debugging.
    ;;
    ;; Usage e.g
    ;; - cf. UNRECOGNIZED-LAMBDA-EXPRESSION vis. DEFUN* and subsq. --
    ;;       in which, this condition slot would contain a list whose
    ;;       CAR is the symbol, DEFUN* and whose CADR is a symbol
    ;;       denoting the function for which the DEFUN* expression is
    ;;       being evaluated. This condition class provides a
    ;;       corresponding :REPORT function, generally illustrative of
    ;;       such application.
    :initform nil
    :type eval-context
    :initarg :context
    :reader macroexpansion-condition-context)))


(declaim (ftype (function (eval-context eval-context)
                          (values eval-context &optional))
                 eval-context-add-suffix))

(defun eval-context-add-suffix (suffix whence)
  ;; NB: Utility for forms that may dispatch to generally signal a
  ;; MACROEXPANSION-CONDITION - add SUFFIX to WHENCE, in a manner
  ;; dependent on the syntax of each value
  (etypecase whence
    (cons
     (typecase suffix
       (cons
        (append whence suffix))
       (null
        (values whence))
       (t
        (append whence (list suffix)))))
    (null
     (typecase suffix
       (cons
        (values suffix))
       (null
        (values nil))
       (t
        (values suffix))))
    (symbol
     (typecase suffix
       (cons
        (append (list whence) suffix))
       (null
        (values whence))
       (t
        (list whence suffix))))))


;; (eval-context-add-suffix 'b 'a)
;; (eval-context-add-suffix 'c '(a b))
;; (eval-context-add-suffix 'c nil)
;;
;; (eval-context-add-suffix nil 'a)
;; (eval-context-add-suffix nil '(a b))
;; (eval-context-add-suffix nil nil)
;;
;; (eval-context-add-suffix '(b c) 'a)
;; (eval-context-add-suffix '(c d) '(a b))
;; (eval-context-add-suffix '(c d) 'nil)


(define-condition lambda-parser-condition (macroexpansion-condition)
  ((lambda-list
    :type list
    :initarg :lambda-list
    :reader lambda-parser-condition-lambda-list)))


(define-condition unrecognized-lambda-expression (lambda-parser-condition)
  ((datum
    :initarg :datum
    :reader unrecognized-lambda-expression-datum))
  (:report
   (lambda (c s)
     (let ((ctxt (macroexpansion-condition-context c))
           (l (lambda-parser-condition-lambda-list c)))
       (format s "~<Unrecognized lambda expression~>~< ~S~>~
~< in~:[~@[ ~A~]~;~{ ~A~}~] lambda list~>~< ~:[()~;~S~]~>"
               (unrecognized-lambda-expression-datum c)
               (consp ctxt) ctxt
               l l)))))

;;; NB
;; (error "Frob~:[~@[: ~S~]~;:~{ ~S~}~]" (consp nil) nil)
;; (error "Frob~:[~@[ : ~S~]~; :~{ ~S~}~]" (consp '(a b)) '(a b))
;; (error "Frob~:[~@[ : ~S~]~; :~{ ~S~}~]" (consp 'a) 'a)

;;; Formatting tests
;; (defparameter *s* (quote  unrecognized-lambda-expression))
;; (error *s* :context '(def-frob frob) :datum "Str" :lambda-list '(a  b "Str"))
;; (error *s* :datum "Str" :lambda-list '(a  b "Str"))
;; (error *s* :datum "Str" :lambda-list nil)


(define-condition unrecognized-lambda-expression-warning
    (warning unrecognized-lambda-expression)
  ())

(define-condition unrecognized-lambda-expression-style-warning
    (style-warning unrecognized-lambda-expression-warning)
  ())

(define-condition unrecognized-lambda-expression-error
    (error unrecognized-lambda-expression)
  ())

;; --

(defconstant* +declare-notype+
    ;; Untyped DECLARE specifiers - by-in-large standard DECLARE
    ;; expression kinds, excepting TYPE, FTYPE, and VALUES
    ;;
    ;; NB This value is used as complimentary to the handling for
    ;; simplified type declarations in such as
    ;;   (declare (simple-string name))
    ;;
  '(dynamic-extent ignore optimize inline special ignorable notinline
    declaration))

;; NB/DNW: (typep 'type 'declaration)
;;
;; TBD: Arbitary implementation-specific storage and portable access for
;;      "The list" of declared DECLARATION kinds, in any/each supported
;;      implementation.


(defconstant* +defun-lambda-param-kwd+
    ;; NB: This provides a more specialized set of symbols than
    ;; CL:LAMBDA-LIST-KEYWORDS itself.
    ;;
    ;; This is believed to represent a general subset of keywords
    ;; available for lambda lists in LAMBDA, DEFUN, DEFGENERIC,
    ;; DEFMETHOD, and other effectively functional forms, in Common
    ;; Lisp.
  '(&optional &rest &key &allow-other-keys))

(defconstant* +defun-lambda-kwd+
    ;; Keywords not per se supproted in conventional DEFGENERIC or
    ;; DEFMETHOD lambda list forms.
    (append +defun-lambda-param-kwd+
            '(&aux)))

(defvar *default-ftype-values* '(values t))
;; ^ NB/Documentation - for purpose of convenience, this symbol can be
;; lexically bound in applications -- to an effect, producing any one or
;; more FTYPE declaration with a single VALUES type, in the effective
;; function signature, using DEFUN* (or similar) without per se
;; providing a VALUES declaration in the forms expression.
;;
;; TBD: Extending this onto arbitrary parameter type declarations
;;      - Note the initial value to NPARSE-FORMS-DECLARATIONS
;;      - TBD Adopting this extension, when implementing support for
;;        {param, type} (EQ) hash tables in the DEFUN* parameter type
;;        declarations parser

#-(or cmucl sbcl)
(declaim (declaration values))
;; ^FIXME Note this in any top-level systems reference documentation.
;;
;; As this system provides a manner of portable support for VALUES
;; declarations, in a manner described above, the behavior of declaring
;; VALUES as a declaration type in implementations not CMUCL or SBCL
;; should not have any negative impact on applications.


;; -- DEFUN* parser functions (internal/ephemeral - see previous annotations)

(declaim (ftype (function (list function)
                          (values &optional))
                walk-forms-declarations)

         (ftype (function (list)
                          (values list (or null string) &optional))
                parse-named-forms-docs)

         (ftype (function (list &optional (or symbol cons))
                          (values list &optional))
                ;; => lambda call-form-signature, formatted vis FTYPE
                ;;    .. containing params information
                parse-lambda-call-params)

         (ftype (function (list t)
                          (values list list &optional))
                ;; => type map (from a list of partially parsed DECLARE forms)
                parse-declared-types)

         (inline #+NIL walk-forms-declarations
                 parse-named-forms-docs
                 parse-lambda-call-params))


(defun walk-forms-declarations (forms callback)
  ;; assumption: any docstring has been removed from FORMS
  ;;
  ;; NB: Handle multiple DECLARE
  (let ((frst (car forms)))
    (cond
      ((and (consp frst) (eq (car frst) 'declare))
       (funcall callback (cdr frst))
       (walk-forms-declarations (cdr forms) callback))
      (t
       (values)))))


(defun parse-named-forms-docs (forms)
  (let ((frst (car forms)))
    (cond
      ((stringp frst)
       (values (cdr forms) frst))
      (t
       (values forms nil)))))


(defun parse-lambda-call-params (lambda-list &optional eval-context)
  ;; NB: This function's return value is used together with
  ;; the return value from PARSE-DECLARED-TYPES, for computing
  ;; an FTYPE declaration in UNPARSE-FTYPE-FUNCTION-DECLARATION-1
  ;;
  ;; This function, as such, provides a value for the
  ;; LPARMS parameter to UNPARSE-FTYPE-FUNCTION-DECLARATION-1
  ;;
  ;; Usage - Within UNPARSE-FTYPE-FUNCTION-DECLARATION-1, this function's return
  ;; value is processed within an iterative form, such that
  ;; will compute a type for each lambda parameter parsed
  ;; out by PARSE-LAMBDA-CALL-PARAMS. For any parameter not
  ;; given an explicit type in any declaration forms,
  ;; UNPARSE-FTYPE-FUNCTION-DECLARATION-1 will provide a resonable default type -
  ;; similarly, for the VALUES element in the resulting
  ;; FTYPE declaration.
  ;;
  ;; For &key arguments, these two functions will utilize a
  ;; specific, list-based data structure:
  ;;   (CONTEXT . (VAR . KEYNAME))
  ;; juxtaposed to the data structure used for other lambda
  ;; list parameters:
  ;;   (CONTEXT . VAR)
  (let ((kwdpkg (load-time-value (find-package '#:keyword)
                                 t))
        ;; NB This function will continue to use a sequence
        ;; for internal storage, to preserve the ordering of
        ;; the original lambda signature when computing the
        ;; effective FTYPE declaration form.
        vars context)
    (declare (type package kwdpkg))
    (dolist (expr lambda-list vars)
      (etypecase expr
        (symbol
         (cond
           ((find expr +defun-lambda-kwd+ :test #'eq)
            (when (eq expr (quote &allow-other-keys))
              ;; NB: This does not match ^ a VARINFO
              ;; bucket - using NIL as a placeholder for a
              ;; parameter name, locally. In usage, the CDR
              ;; of the resulting expr should be ignored - as
              ;; for the &allow-other-keys context, when
              ;; unparsing the effective FTYPE declaration.
              ;;
              ;; In short: This keyword affects the lambda
              ;; signature and any corresponding call forms,
              ;; in a manner, by side-effect. It must appear
              ;; in any FTYPE declaration for a function in
              ;; which it appears in the function's lambda
              ;; list.
              (npushl (cons expr nil) vars))
            ;; Update the parser context - NB this in itself
            ;; does not verify, warn or err for unsupported
            ;; lambda list syntax e.g duplicate keywords
            (setq context expr))
           ((eq context (quote &key))
            ;; NB: already parsed the &KEY keyword itself
            (let ((key (intern (symbol-name expr)
                               ;; NB Simple interpolation for
                               ;; an &KEY keyword symbol
                               kwdpkg)))
              (npushl (cons context (cons expr key))
                      vars)))
           ;; other symbol
           (t (npushl (cons context expr) vars))))
        (cons
         ;; assumptions: EXPR is a CONS - thus decribing an
         ;; &OPTIONAL or &KEY element of the effective
         ;; lamabda list FTYPE signature.
         ;;
         ;; &AUX elements are ignored, here, for purpose of
         ;; constructing the effective FTYPE signature.
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
             ;; NB DEFSIGNSTURE KIND handling
             (t (warn 'unrecognized-lambda-expression-warning
                      ;; NB: Only reached for CONS type lamba
                      ;; element exprs
                      :datum expr
                      ;; FIXME note that the CONTEXT (EVAL-CONTEXT)
                      ;; should be provided by any calling form
                      :context eval-context
                      :lambda-list lambda-list)))
           ))))))


(defun parse-declared-types (decls env)
  ;; NB: ENV param - used only in FIND-CLASS, below

  ;; returns: Type table (assoc), Values expr if present in DECLS

  ;; NB: Any simplified type declarations in effect shadowing
  ;; known declaration kinds - vis +DECLARE-NOTYPE+ - will
  ;; be, as by side effect, ignored in this portable
  ;; implementation.

  (let (typed vdecl)

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
      ;; process (and parse) each element of DECLS per declaration kind
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

           ;; Note that DEFUN* will utilize the CMUCL (or
           ;; SBCL) lambda VALUES declaration in producing a
           ;; top-level FTYPE declaration - thus, supporting
           ;; such a convention of strong typing, insofar as
           ;; in FTYPE declarations, for all implementations.
           ;;
           ;; Implementation-specific optimizations would
           ;; be beyond the scope of this comment
           (setq vdecl expr))
          ((eq kind 'ftype)
           ;; NB a manner of a subset of TYPE decls
           ;; Not parsed here (No-Op)
           #+NIL (npushl expr ftyped)
           )
          ((find kind +declare-notype+ :test #'eq)
           ;; No-Op
           #+NIL
           (npushl expr cldecl))
          ((find-class kind nil env)
           ;; parse as a simplified type decl
           (dolist (var (cdr expr))
             (npushl (cons var kind) typed)))
          (t ;; No-Op
           #+NIL
           (npushl expr other)))))
    ;; NB: OTHER may contain non-class-name simplified type
    ;; declarations (FIXME? Portably?) and arbitrary
    ;; function-specific declarations

    ;; Otherwise expressed ...
    ;;
    ;; The DEFUN* FTYPE parser, in its present
    ;; implementation, will not recognize any simplified type
    ;; declarations that do not represent class names.

    ;; Furthermore ...
    ;;
    ;; This implementation of DEFUN* will not provide any
    ;; implementation-specific sections of program source
    ;; code - vis a vis the type system implementation, in
    ;; any single Common Lisp implementation.
    ;;
    ;; Assumption: Any declarations in OTHER will not provide
    ;; any locally accessible type information about the
    ;; LAMBDA list

    ;; ...
    (values typed vdecl)))


(defun unparse-ftype-function-declaration-1 (params-call-form
                                             type-map values-decl)
  (declare (type list type-map))
                                        ; FIXME Continue source review
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
  ;; environment, a non-null lexical environment -- e.g
  ;;  PARSE-NAMED-LAMBDA (NAME LAMBDA FORMS KIND ENV) => FTYPE, DOCS, FORMS

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
  ;; parameters that would be provided, as via call forms,
  ;; from within a calling lexical environment.

  (let (param-spec context)
    ;; Re-map PARAMS-CALL-FORM x TYPE-MAP into a lambda-like PARAM-SPEC
    (dolist (bkt params-call-form)
      ;; NB: for any &key parameter, the CDR of BKT will be
      ;; a list -- as produced within PARSE-LAMBDA-CALL-PARAMS --
      ;; in a general format: (CONTEXT . (VAR . KEYNAME))
      ;; with CONTEXT being, as such `&KEY`
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
                  ;; NB: type-map un-parsing here
                  (type-n (position varname type-map
                                    :key #'car
                                    :test #'eq))
                  ;; FIXME: Parameterize this default type
                  (type t))
             (when type-n
               ;; NB: else, initial type 'T' is used
               (setq type (cdr (nth type-n type-map))))

             (case ctxt
               ;; FIXME use a default type LIST for any
               ;; &REST param
               (&key
                (let ((kwd (cdr varinfo)))
                  ;; Note the remark about "Specialized &key
                  ;; forms" in PARSE-LAMBDA-CALL-PARAMS local defun
                  (npushl (list kwd type) param-spec)))
               (t
                (npushl type param-spec))))
           )))) ;; DOLIST
    ;; return from the unparser
    (values `(function ,param-spec ,values-decl))))
;; --


(defun unparse-ftype-function-declaration (lambda-list forms
                                           &optional eval-context
                                             environment)
  ;; return an unadorned FUNCTION type declaration in a syntax as may
  ;; be used for an FTYPE declaration, per LAMBDA-LIST and any
  ;; declarations directly in FORMS
  (let ((lparms (parse-lambda-call-params lambda-list eval-context))
        type-map values-type)
    (labels ((build-type-map (decl-rest)
               (multiple-value-bind (map vdecl)
                   (parse-declared-types decl-rest environment)
                 (setq type-map
                       (nconc type-map map))
                 (when vdecl
                   (setq values-type vdecl)))))
      (walk-forms-declarations forms #'build-type-map)
      (values
       (unparse-ftype-function-declaration-1
        lparms type-map (or values-type *default-ftype-values*))
       ))))




(defun unparse-ftype-named-declaration (name lambda-list forms
                                        &optional eval-context
                                          environment)
  ;; NB: Generalized onto the LAMBDA case
  ;;
  ;; See also/TBD: LABELS*
  (multiple-value-bind (forms docs)
        (parse-named-forms-docs forms)
      (declare (ignore docs)) ;; as though!
      (multiple-value-bind (ftype-functype)
          (unparse-ftype-function-declaration lambda-list forms
                                              (eval-context-add-suffix
                                               name eval-context)
                                              environment)
        (values `(ftype ,ftype-functype ,name)))))

;; --


(defmacro defun* (name lambda &rest forms &environment env)
  ;; NB: FORMS should not be destructively modified in the
  ;; macroexpansion, as it may represent constant data within a compiler
  ;; environment. Any "New Lists" returned in the parser implementation
  ;; may be destructively modified, however.
  ;;
  ;; Ideally, this implementation should produce only a minimum of "New
  ;; Objects," for processing the LAMBDA form and any declarations
  ;; parsed from the provided FORMS.

    (let ((ftype
           (unparse-ftype-named-declaration name lambda forms
                                            (quote defun*)
                                            env)))
      `(progn
         (declaim ,ftype)
         (defun ,name ,lambda ,@forms)
         )))


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
  "Find the DIGIT-CHAR of DIGIT in the string WHERE"
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


;; ----

(define-condition compile-condition ()
  ;; FIXME: Portable "In-what-section" declaration
  ())

(define-condition lambda-compile-condition (compile-condition)
  ((form
    :initarg :form
    :initform nil
    :reader lambda-compile-condition-form)))

#+NIL ;; unused here
(define-condition lambda-compile-warning  (warning compile-condition)
  ())


(define-condition lambda-compile-error (error lambda-compile-condition)
  ()
  (:report
   (lambda (c s)
     (format s "~<Error when compiling lambda form~>~< ~S~>"
             (lambda-compile-condition-form c)))))

;; ----

;; TBD: Use UNPARSE-FTYPE-FUNCTION-DECLARATION for producing a second return
;; value in the macroexpansion of LAMBDA* - Refer to documentation, above.


(defmacro lambda* (args &body body &environment env)
  ;; NB: Reimplemented subsq,
  ;; pursuant of availability of WITH-ENCAPSULATED-CONDITIONS

  ;; NB: This macro was going to be named MK-LAMBDA.
  ;;
  ;;     This macro has been named LAMBDA* mostly due to concerns
  ;;     entailed of some source editing environments.
  ;;
  ;; NB in earlier changesets, in the LTP-Common source repository
  ;; - common-misc.lisp
  ;;   - COMPILE** (NB: REMOVED) [NB: used DEFMACRO]
  ;;   - [FIXME/NB] FORMAT-CONDITION (Is not PRINT-OBJECT)
  ;;   - COMPILE* (NB: REMOVED) [NB: used DEFUN]
  ;;

  ;; NB: Unlike the function, COMPILE, the third return value from the
  ;; macroespansion of this function will denote a function declaration,
  ;; in a syntax similar onto FTYPE. That function declaratiion will
  ;; have been derived from the lambda ARGS and any TYPE and VALUES
  ;; declarations preceding evaluation forms in the lambda BODY
  ;; forms. The function declaration, as such, is derived directly in
  ;; the macro function itself.
  ;;
  ;; The same declaration will be used in a THE form effectively
  ;; wrapping the form denoting the resulting lambda fujction, itself.

  (with-symbols (%warnings %errors %form %fn %functype)
    (let ((functype
           (unparse-ftype-function-declaration args body 'lambda* env)))
      `(let ((,%form (quote (lambda ,args ,@body)))
             ,%warnings ,%errors)
         (handler-bind ((warning #'(lambda (c) (npushl c ,%warnings)))
                        (error #'(lambda (c) (npushl c ,%errors))))
           (let ((,%fn (coerce ,%form 'function))
                 (,%functype ,(list 'quote functype)))
             (cond
               (,%errors
                ;; NB: This may not ever be reached, in
                ;; some implementations.
                (error 'lambda-compile-error
                       :form ,%form))
               (,%warnings (values (the ,functype ,%fn) ,%warnings
                                   ,%functype))
               (t (values (the ,functype ,%fn) nil ,%functype)))))))))

;; ^ Operate similar to COMPILE, principally, for anonymous lambda forms
;;   but without any mangling of the lexical environment in which the
;;   function is produced. Thus, the HANDLER-BIND forms many generally
;;   be accessible, at least for WARNING.
;;
;;   Furthermore, this ensures that all warnings will be emitted in the
;;   second return value.
;;
;;   Rather than using COMPILE, this approach does not require special
;;   binding onto *BREAK-ON-SIGNALS* for some implementations.
;;
;;   COMPILE itself may inject a null lexical environment, even when
;;   compiling anonymous lambda forms, on some implementations. This may
;;   have an unwanted side effect of making any condition handlers in
;;   the calling environment unavailable in the context of the call to
;;   COMPILE.


#+NIL
(eval-when ()
  ;; FIXME - this source system needs an update for hacking onto Emacs font-lock

  (lambda* () unbound-symbol)
  ;; ^ warning returned - in duplicate, under the redefined LAMBDA* (TBD/FIXME)

  (lambda* #:fail)
  ;; ^ NB: Even here, the error may not get caught by the HANDLER-BIND w/ SBCL
  ;;       ... possibly, due to the implementation injecting a NULL-LEXENV
  ;;       during the internal LAMBDA eval -- quite similar to why this
  ;;       workaround was developed for that behavior of COMPILE in SBCL.

  (lambda* (&key ((((unreachable))))  (funcall #:fail)))
  ;; ^ similarly, the ERROR handler is not reached

  (lambda* () (funcall #:fail))
  ;; ^ a warning is returned, no error


  (funcall (lambda* () (declare (values (member 4))) (+ 1 3)))
  ;; => 4

  (funcall (lambda* () unbound-symbol (+ 1 3)))
  ;; ^ error outside of the LAMBDA* eval

  )

;; --

;; TBD - cf. LAMBDA* => COMPILE

#+TBD
(defmacro with-encapsulated-conditions ((&rest types) &body forms)
  ;; FIXME
  ;;
  ;; TBD - Wrapping for *BREAK-ON-SIGNALS* and *DEBUGGER-HOOK*
  (with-symbols (register-condition c whence restart values)
    (let ((tabl (mapcar #'(lambda (typ)
                            (declare (type symbol typ))
                            (cons typ
                                  (make-symbol
                                   (concatenate 'simple-string
                                                (symbol-name typ) "-"
                                                (mk-lf
                                                 (symbol-name '#:storage))))))
                        types)))
      `(let ,(mapcar #'(lambda (spec)
                         (destructuring-bind (typ . storage) spec
                           (declare (ignore typ))
                           `(,storage (make-array 0 :fill-pointer 0
                                                  :adjustable t))))
                         tabl)
         (labels ((,register-condition (,c ,whence)
                    (unless (find ,c (the (array t (*)) ,whence) :test #'eq)
                      (vector-push-extend ,c ,whence))))
           (let (#+NIL (*break-on-signals* (quote (or ,@types)))
                       ,values
                       )
             (handler-bind ,(mapcar #'(lambda (spec)
                                        (destructuring-bind (typ . storage) spec
                                          `(,typ (lambda (,c)
                                                   (,register-condition ,c ,storage)
                                                   (let ((,restart
                                                          (find-restart 'continue ,c)))
                                                     (when ,restart
                                                       (invoke-restart ,restart #+NIL ,c)))))))
                                    tabl)
               ;; TBD - Wrapped/impl-specific binding for *DEBUGGER-HOOK*
               ;; so as to capture all implementation-wrapped conditions
               ;; under *BREAK-ON-SIGNALS*
               (setq ,values (multiple-value-list (progn ,@forms)))
               (values-list
                (append (multiple-value-list (progn ,@forms))
                        (list
                         ,@(mapcar
                            #'(lambda (spec)
                                (destructuring-bind (typ . storage) spec
                                  (declare (ignore typ))
                                  `(coerce ,storage 'list)))
                            tabl))))
               )))))))

#+NIL
(eval-when ()

(macroexpand-1 (quote

(with-encapsulated-conditions (warning error)
  (lambda* () (frob #.(make-symbol "Unbound"))))

;; ^ TBD "Quirks" - No minibuffer display or Emacs *Messages* buffer I/O (??)
;;   when eval w/ SBCL 1.4.16.debian, SLIME/Swank 4.19.0-2-amd64, Emacs 26.1
;;
;; It "Works-Out OK" when input directly to the REPL however ....
;; .. albeit, while the wrapped comditions are fairly noisy there.

))

#+FIXME
(with-encapsulated-conditions (warning error)
  ;; FIXME - TBD (SBCL)
  ;;
  ;; DNW - CCL - TBD/FIXME - Errs but the error is not caught
  (funcall (lambda* () unbound-symbol (+ 1 3))))


(with-encapsulated-conditions (warning error)
  ;; ^ DNW insofar as capturing the CERROR after interactive 'continue' :
  (warn "FROB")
  ;; FIXME - the following CERROR call may be evaluated twice, seen in:
  ;;  - CCL w/ SLIME inline eval
  ;;  - CCL w/ SLIME REPL
  ;;  - SBCL, similarly
  (cerror "FROB" (make-condition 'condition))
  12332
  )

)


;; --

#+TBD
(defmacro lambda* (args &body body &environment env)
  ;; Redefinition of LAMBDA*
  ;; subsq. of the availability of WITH-ENCAPSULATED-CONDITIONS

  ;; FIXME -
  ;; - Retain documentation from the original LAMBDA* definition
  ;; - Revise return form of WITH-ENCAPSULATED-CONDITIONS -> "MV-APPEND" [DONE]
  ;; - Note that the implementation may return effective duplicates of
  ;;   conditions produced in REPL evaluation of some calls to this
  ;;   redefined LAMBDA* - TBD, possible casual correlation due to
  ;;   inline compilation in the (COERCE LIST 'FUNCTION) call

  (with-symbols (%form %fn %functype)
    (let ((functype
           (unparse-ftype-function-declaration args body 'lambda* env)))
      `(let ((,%form (quote (lambda ,args ,@body))))
         (with-encapsulated-conditions (warning error)
           (let* ((,%fn (coerce ,%form 'function))
                  (,%functype ,(list 'quote functype)))
             (values (the ,functype ,%fn) ,%functype)))))))

;;   (funcall (lambda* () (declare (values (member 4))) (+ 1 3)))

;;
;;;;
;;;; Note that this may be handled more effectively, in the LAMBDA*
;;;; redefinition ... under some implementations [CCL]
;;;;
;;
;; (lambda* (&key ((((unreachable))))  (funcall #:fail)))
