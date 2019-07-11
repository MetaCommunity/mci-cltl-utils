;; spec.lisp - local prototypes for method specialization onto MOP
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------


(in-package #:ltp/common/mop)

;; rcs -i -U spec.lisp
;; ci spec.lisp
;;
;; co spec.lisp

;; NB: Development paused, while developing ltp-main:src/main/lsp/mt/
;;     - guarding.lisp - WITH-WRITE-GUARD, OBJECT-POOL, GUARDED-FUNCALL
;;     - defportable.lisp - DEFSECTION [TBD], DEFSIGNATURE, DEFIMPLEMENTATION
;;
;; NB: With regards to initfunction definition, LTP/COMMON:LAMBDA* avl.
;;
;; TBD: Docstrings w/i a TeX environment


;; --------------------

;; NB: FIXME WHILE is used only once, below
;;
;; Consider moving these two macros to LTP/COMMON

(defmacro while* ((clause &optional retv) &body body)
  `(loop (or ,clause (return ,retv))
      (progn ,@body)))

;; (let ((n 10)) (while* ((not (zerop n)) n) (decf n)))

;; (let ((n 10)) (while* ((not (zerop n)) n) (when (= n 5) (return -10)) (decf n)))



(defmacro while (clause &body body)
  `(while* (,clause) ,@body))


;; (let ((n 10)) (while (not (zerop n)) (decf n)) n)


;; --------------------


#+NIL ;; unused macro, in this revision
(defmacro push-nth (n val whence)
  (with-symbols (%n %whence nc)
    `(let ((,%n ,n)
           (,%whence ,whence))
       (declare (type (integer 0) ,%n))
       (cond
         ((zerop ,%n)
          (setf ,whence (cons ,val ,%whence)))
         (t
          (let ((,nc (nthcdr (1- ,%n) ,%whence)))
            (cond
              ((consp ,nc)
               (push ,val (cdr ,nc))
               ,%whence)
              (t (error
                  "~<Cannot PUSH-NTH for index ~D of ~S :~>~
~< NTH CDR ~D => ~S is not a CONS~>"
                  ,%n ,%whence (1- ,%n) ,nc)))))))))

;; (let ((v (list 'a 'b))) (push-nth 0 t v) v)

;; (setq *print-circle* t)

;; (let ((v (list 'a 'b))) (push-nth 1 t v) v)

;; (let ((v (list 'a 'b))) (push-nth 2 t v) v)

;; FAIL NB:
;; (let ((v (list 'a 'b))) (push-nth 3 t v) v)



;; --------------------

;; Trivial prototyping for specialized method dispatching
;; with minimal consing in generic function calls

;; FIXME move to spec-proto.lisp and update reference in spec-mop.lisp


(defun mk-expandable-vec ()
  (make-array 0 :fill-pointer t :adjustable t))

#+NIL ;; unused here
(defun reset-expandable-vec (which)
  (setf (fill-pointer which) 0))



(eval-when ()

(let ((specialization-table (mk-expandable-vec))
      ;; ^ NB: The call forms actually coerce this to a list

      #+TD (call-table (mk-expandable-vec))
      )
  (labels ((mk-specialization-row (args)
             ;; create and store an analogy to a method specialization
             (vector-push-extend (coerce (mapcar #'compute-class  args)
                                             'simple-vector)
                                 specialization-table))


           (compute-call-info-for-n (cls specializer-offset known)
             ;; compute a list of all specializations for parameter of
             ;; class CLS at OFFSET in the specializable parameter list,
             ;; for an initial set of KNOWN specializations

             (let (;;; (start 0)
                   (depth 0)
                   (%known)
                   tmp-1 tmp-2
                   )


               #+DEBUG (warn "OK ~S KNOWN ~S" specializer-offset known)

               (dolist (c (class-precedence-list cls))
                 (let ((known known))
                   (while (progn
                            #+DEBUG (warn "ITERATE ~D ~D ~D" specializer-offset depth (length known))
                            known)
                     ;; (warn "ITERATE w/ KNOWN ~S" known)
                     (let ((n (position c (the cons known)
                                        :key #'(lambda (row)
                                                 (svref (cdr row) specializer-offset))
                                        :test #'eq
                                        ;; :test #'subtypep
                                      ;;; :start start
                                        )))
                       (cond
                         (n
                          #+DEBUG
                          (warn "~<GOT ~S for depth ~S of arg nr. ~S : ~>~< ~S~>"
                                n depth
                                specializer-offset
                                (nth n known))
                          (setq tmp-1 (nthcdr n known)
                                tmp-2 (car tmp-1))
                          (setf (car tmp-2) (nconc (car tmp-2) (list depth)))
                          (push tmp-2 %known)
                          (setq ;;; start n
                           known (cdr tmp-1)
                           ))
                         ;; else return from while
                         (t ;; (setq known nil)
                          (return))))) ;; WHILE
                   )
                 (incf depth)) ;; DOLIST

               #-NIL (values %known)
               #+NIL
               (sort %known #'(lambda (inst-a inst-b)
                                ;; generic composite sort-by-maximal-precedence
                                ;; for a generic function of three
                                ;; specializable params (May not be 110% MOP)
                                (destructuring-bind (a-n-1 &optional a-n-2 a-n-3)
                                    (car inst-a)
                                  (destructuring-bind (b-n-1 &optional b-n-2 b-n-3)
                                      (car inst-b)
                                    ;; FIXME no type optimization here.
                                    ;; When non-nil, each of these is an
                                    ;; unsigned fixnum
                                    (cond
                                      ((and a-n-3
                                            (= a-n-1 b-n-1)
                                            (= a-n-2 b-n-2))
                                       ;; NB This is the only case that
                                       ;; matters - call this from an
                                       ;; upper call, where A-N-3 and
                                       ;; A-N-2 := always avl
                                       (< a-n-3 b-n-3))

                                      ((and a-n-2
                                            (= a-n-1 b-n-1))
                                       (< a-n-2 b-n-2))
                                      (t (< a-n-1 b-n-1))
                                      )))))
               ))


           (compute-call-info-for (a b c)
             ;; compute for each arg
             ;; NB: this labels function can be computed with a template
             (let ((reachable  (mapcar #'(lambda (spec)
                                           (cons nil spec))
                                       (coerce specialization-table 'list))))

               (dolist (hack (list (list a 0 reachable) (list b 1) (list c 2))
                        #+NIL (values reachable))
                 ;; FIXME - Cheap convenience hack for iterative computation
                 ;; onto a static form. Uses needless CONS initialization

                 (destructuring-bind (param-class param-offset
                                                  &optional (%reachable reachable)) hack
                   #+DEBUG
                   (warn "Call for ~D with ~D reachable"
                         param-offset (length reachable))

                   (setq reachable
                         (compute-call-info-for-n param-class param-offset %reachable))
                   #+DEBUG
                   (warn "Call for ~D got reachable ~S" param-offset reachable)
                   ))

               (sort reachable #'(lambda (inst-a inst-b)
                                ;; generic composite sort-by-maximal-precedence
                                ;; for a generic function of three
                                ;; specializable params (May not be 110% MOP)
                                (destructuring-bind (a-n-1 a-n-2 a-n-3)
                                    (car inst-a)
                                  (destructuring-bind (b-n-1 b-n-2 b-n-3)
                                      (car inst-b)
                                    (declare (type (integer 0 #.most-positive-fixnum)
                                                   a-n-1 a-n-2 a-n-3
                                                   b-n-1 b-n-2 b-n-3))
                                    ;; NB: As sparse as this may seem,
                                    ;; it work out in tests.
                                    (cond
                                      ((and (= a-n-1 b-n-1)
                                            (= a-n-2 b-n-2))
                                       (< a-n-3 b-n-3))
                                      ((= a-n-1 b-n-1)
                                       (< a-n-2 b-n-2))
                                      (t
                                       (< a-n-1 b-n-1))
                                      )))))


               ))

           (compute-call-for (a b c)
             (compute-call-info-for (class-of a) (class-of b) (class-of c)))
           )


    (mk-specialization-row '(array fixnum t))
    (mk-specialization-row '(string integer t))
    (mk-specialization-row '(string fixnum t))
    (mk-specialization-row '(array integer t))
    (mk-specialization-row '(t fixnum t))
    (mk-specialization-row '(t fixnum cons))
    (mk-specialization-row '(t integer t))
    (mk-specialization-row '(t t symbol))
    (mk-specialization-row '(array t t))
    (mk-specialization-row '(string t t))
    (mk-specialization-row '(t t t))
    (mk-specialization-row '(t t list))

    (mk-specialization-row '(integer t t))
    (mk-specialization-row '(fixnum t t))


    (list (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                  ;; should be the entire set, sorted
                  (compute-call-for "Frob" 5 nil))

          (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                   ;; should be a sorted complete subset
                  (compute-call-for #() 5 nil))


          (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                   ;; should be a sorted complete subset
                  (compute-call-for 5 5 '(a b c)))

          )

    ;; (setq *print-circle* nil)

    ;; (pushnew :debug *features* :test #'eq)

    ))

;; =>
;; ((3 0 4) STRING FIXNUM T)
;; ((3 1 4) STRING INTEGER T)
;; ((3 5 4) STRING T T)
;; ((6 0 4) ARRAY FIXNUM T)
;; ((6 1 4) ARRAY INTEGER T)
;; ((6 5 4) ARRAY T T)
;; ((8 0 4) T FIXNUM T)
;; ((8 1 4) T INTEGER T)
;; ((8 5 1) T T SYMBOL)
;; ((8 5 2) T T LIST)
;; ((8 5 4) T T T)
;; , ...

)

;; see also: spec-mop.lisp

#+NIL
(defgeneric compute-call-lambda (op)) ;; cf COMPUTE-CALL-INFO-FOR... ^
;; ^ cf. SET-FUNCALLABLE-INSTANCE-FUNCTION
;; ^ nb: May be called whenver a defop-method is added or removed from
;;       the generic-op
;;
;;
;; ^ In lieu of MOP COMPUTE-DISCRIMINATING-FUNCTION

;; NB, "Goal:" Do not make any more consing. to determine "applicable methods"


;; -----

#|

Ed. NB: The following was defined originally in an effort to provide a
complete semantic model for lambda list expressions. It had provided, in
version one, a preliminary support for the subset of labmda list syntax
as supported in standard generic functions.

The API is still in revision. Though it may lastly resemble not much
more than a specially typed list-like API, but perhaps it may serve to
be of some use in application support.

|#

(eval-when (:compile-toplevel :execute)
  (defmacro lform (val)
    `(load-time-value ,val t)))


(defstruct (lambda-element
             (:constructor))
  (name
   (lform (make-symbol "Unbound Name"))
   :type symbol
   :read-only t)
  (expr-next
   ;; FIXME: This comprises a redundant slot-vlue except across
   ;; instances of LAMBDA-KEYWORD
   nil
   :type (or lambda-element null)
   :read-only nil)
  )


(defmethod print-object ((object lambda-element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (lambda-element-name object) stream)))


(defstruct (lambda-keyword
             (:include lambda-element)
             (:constructor
              make-lambda-keyword (name))))


(defstruct (param
             (:include lambda-element)
             (:constructor))
  (next ;; NB: Next parameter for purposes of call-form eval
   nil ;; indicating, in effect, "End of lambda list"
   :type (or param null)
   ;; NB: Theoretically, PARSE-LAMBDA-SIGNATURE could be updated to
   ;; allow for this slot to be defined as not read-only. It's been
   ;; defined as being not read-only, due to how PARAM subclases
   ;; will be initialized sequentially, within
   ;; PARSE-LAMBDA-SIGNATURE.
   ;l
   ;; As to why this slot is not defined as read-only, remarks in
   ;; the source form of PARSE-LAMBDA-SIGNATURE may serve to detail
   ;; that decision.
   ;;
   ;; If any application destructively modifies the value of this slot,
   ;; in any procedure external to PARSE-LAMBDA-SIGNATURE, the
   ;; behaviors will be unspecified for any later procedures operating
   ;; as to unparse a lambda list from any LAMBDA-SIGNATURE containing
   ;; such a modified PARAM.
   ;;
   ;; NB: Together with the FIRST-PARAM slot of LABMDA-SIGNATURE, these
   ;; data structures provide an interface not entirely unlike Lisp LIST
   ;; objects, insofar as for LIST CAR and CDR accessors. Somewhat
   ;; unlike CDR, however, the PARAM-NEXT accessor returns a PARAM
   ;; object, as whenever it does not return NIL.
   ;;
   ;; One might consider that this may be fairly trivial, for any data
   ;; sturctures that may be defined initially as being singularly
   ;; linked.
   :read-only nil)
  )

;; FIXME: Although this API might be expanded beyond any point of
;; obvious utility, there may be support provided for storage of the
;; actual lambda list keywords symbols within the effective sequence of
;; PARAM-NEXT values. Otherwise, it may be impossible to maintain an
;; accurate, parsed representation of such as:
;; - &KEY without any subsequent keyword parameters
;; - &OPTIONAL without any subsequent optional parameters
;;
;; Furthermore, such an addition should serve to simplify the unparsing.
;;
;; Albeit, this API then would provide  something very much like lambda
;; lists in a list format, albeit with some additional semantic
;; properties stored for any later application.


(defstruct (initialized-param
             (:include param)
             (:constructor))
  ;; NB: Usage by extension vis a vis CL:&KEY, CL:&OPT params
  (initform
   (lform (make-symbol "Unbound Initform"))
   :type t
   :read-only t)
  (initfunction
   ;; NB: NIL in this slot implies that the initorm slot can be ignored
   nil
   :type (or function null)
   :read-only t)
  ;; NB this needs a value-p-p slot, as the structure slot cannot be unbound
  (value-p-name
   (lform (make-symbol "Unbound Value-P-Name"))
   :type symbol
   :read-only t)
  (value-p-p
   ;; NB: To be construed as indicating that the value-p-name was
   ;; provided from evaluation of source forms, rather than being
   ;; initialized by default as in PARSE-LAMBDA-SIGNATURE
   ;; (May be removed in a subsequent changeset)
   nil
   :type boolean
   :read-only t))


(defstruct (param-subset
             (:constructor))
  (members
   (lform (make-array 0))
   :type simple-vector
   :read-only t))

;; --

(defstruct (required-param
             (:include param)
             (:constructor make-required-param
                           (name))))

(defstruct (required-subset
             (:include param-subset)
             (:constructor %mk-required-subset
                           (members))))


(defstruct (optional-param
             (:include initialized-param)
             (:constructor make-optional-param
                           (name
                            &key
                            ;; NB: Assuming the environment for the
                            ;; initfunction is "Just handled," portably,
                            ;; in applications. also for KEY-PARAM
                            initform
                            initfunction
                            value-p-name
                            value-p-p))))

(defstruct (optional-subset
             (:include param-subset)
             (:constructor %mk-optional-subset
                           (members))))


(defstruct (rest-param
             (:include param)
             (:constructor make-rest-param
                           (name &optional (kind (quote &rest)))))
  (kind
   (quote &rest)
   :type symbol ;; NB narrower type (member &rest &body) ; typically
   :read-only t))


(defstruct (key-param
             (:include initialized-param)
             (:constructor make-key-param
                           (name
                            &key
                            initform
                            initfunction
                            value-p-name
                            value-p-p
                            (keyword-name
                             (intern (symbol-name name) :keyword)))))
  (keyword-name
   (lform "Unbound Keyword Name")
   :type symbol
   :read-only t))


(defstruct (key-subset
             (:include param-subset)
             (:constructor %mk-key-subset
                           (members &optional allow-other-keys)))
  (allow-other-keys
   nil
   :type boolean
   :read-only t))


#+NIL ;; unused as yet - see annotations, subsq.
(defstruct (aux-param
             (:include initialized-param)
             (:constructor make-aux-param (name #:FIXME_INIT))))


(defstruct (other-subset
             (:include param-subset)
             (:constructor %mk-other-subset
                           (members))))


(defstruct (lambda-signature
             (:constructor %mk-lambda-signature
                           (&key
                            lambda-list
                            ((:first first-param) nil)
                            ((:required required-subset) nil)
                            ((:optional optional-subset) nil)
                            ((:keyword key-subset) nil)
                            ((:other other-subset) nil)
                            ((:rest rest-param) nil)
                            )))
  ;; Ed. NB: See remarks about the design of this lambda list parser
  ;; model, in the PARSE-LAMBDA-SIGNATURE source form

  (first-param
   ;; FIXME: May actually be a LAMBDA-ELEMENT
   nil
   :type #+NIL param #-NIL lambda-element
   :read-only t)

  (lambda-list
   ;; NB: Redundant storage, but usable for PRINT-OBJECT methods
   (lform (list (make-symbol "Unbound Lambda List")))
   :type list
   :read-only t)

  (required-subset
   nil
   :type (or required-subset null)
   :read-only t)
  (optional-subset
   nil
   :type (or optional-subset null)
   :read-only t)
  (key-subset
   nil
   :type (or key-subset null)
   :read-only t)
  (rest-param
   nil
   :type (or rest-param null)
   :read-only t)
  (other-subset
   ;; set of param-typed objects e.g for representation of  &aux, &env,
   ;; &whole  and various implementation-specific expressions
   ;;
   ;; cf. OTHER-HANDLER application (below)
   ;;
   ;; Of course, &ENV and &WHOLE would represent other-subset parameters
   ;; of some singular meaning - there being at most one of those
   ;; parameters available in each of some standard lambda syntaxes.
   ;;
   ;; For purposes both of brevity, this LAMBDA-SIGNATURE class will not
   ;; provide singular storage for &ENV and &WHOLE parameters. While it
   ;; does provide singular storage for the &REST parameter of a lambda
   ;; list, it was not believed - at the time of this implementation -
   ;; that &ENV and &WHOLE parameters represented any sufficiently
   ;; generalized feature of lambda-list syntax, sufficient for such
   ;; parameters to be represented individually, within a
   ;; LAMBDA-SIGNATURE class. The existence of those parameters, in a
   ;; LABMDA-SIGNATURE, may be determined either by parsing the
   ;; OTHER-SUBSET directly, or by parsing in a manner beginning at the
   ;; LAMBDA-SIGNATURE-FIRST-PARAM then to each PARAM-NEXT until either
   ;; arriving at the respective PARAM or else, arriving at a NULL
   ;; PARAM-NEXT.
   ;;
   ;; ----
   ;; Additional Documentation Note:
   ;;
   ;; In so far, this lambda list parser model provides a direct support
   ;; only for the generalized lambda list syntax supported in standard
   ;; generic functions.
   ;;
   ;; It was thought that this model should be designed to permit for
   ;; reusability and extensibility. As such, the original design of
   ;; this parser model has been updated to provide the OTHER-SUBSET
   ;; field, for storage of arbitrary lambda list parameters within a
   ;; parsed LAMBDA-SIGNATURE. Moreover, this design has been updated to
   ;; provide a semantics of singly-linked lists for lambda PARAM
   ;; objects, such that may serve to provide a more simplified approach
   ;; for LAMBDA-SIGNATURE unparsing.
   ;;
   ;;
   nil
   :type (or other-subset null)
   :read-only t)
  ;; TBD in extensions:
  ;; - AUX-SUBSET - specifically for anonymous lambdas & defun forms
  ;; - ENV-PARAM - specifically for macro lambda forms
  ;; - WHOLE-PARAM - cf. SETF forms and similar
  ;; ... and interop. for implementation-specific lambda forms
  ;;
  ;; NB/Topic: User Interfaces & Accessibility for Lambda Model Definitions
  ;;
  ;; NB: This was originally defined for supporting implementation of
  ;; GENERIC-OP, begeinning with a method onto the locally defined generic
  ;; function, COMPUTE-FTYPE-PARAMS-TYPE. As such, the syntax supported
  ;; with this model, in its baseline features, was limited to the
  ;; lambda list syntax supported for standard generic functions. As a
  ;; happy side effect, this model supports a generlaized lambda list
  ;; syntax such that may be fairly common across specialized lambda
  ;; list kinds.
  ;;
  ;; Insofar as providing an OTHER-SUBSET semantics, together with the
  ;; addition of the optional OTHER-HANDLER argument for each of the
  ;; functions, PARSE-LAMBDA-SIGNATURE and UNPARSE-LAMBDA-SIGNATURE, this
  ;; parser model now provides a methodology for extending the existing
  ;; support to allow for other lambda list syntaxes - assuming,
  ;; specialized extensions of the PARAM class, correspondingly, and
  ;; some specific behaviors in the OTHER-HANDLER functions for each of
  ;; those call forms.
  )


(defmethod print-object ((object lambda-signature) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (princ (lambda-signature-lambda-list object)
           stream)))


;; ----


(declaim (ftype (function
                 (lambda-signature &optional
                                   (or null function)
                                   (or null function)
                                   (or null function))
                 (values list &optional))
                unparse-lambda-signature))


;; NB: COMPUTE-CALL-FORM cf. GENERIC-OP - Generic prototype defined, below

(defun unparse-lambda-signature (signature &optional
                                        other-unparser
                                        other-initialize
                                        other-finalize)

  ;; FIXME: Needs further update
  ;;
  ;; - Empty &KEY and &OPTIONAL subsets not being correctly unparsed.
  ;;
  ;;   - The present LAMBDA-SIGNATURE definition may need to be updated
  ;;     for those special conditions of syntax

  ;; NB: OTHER-UNPARSER - if provided, a function accepting two
  ;; arguments, i.e a PARAM object not handled below, and the SIGNATURE
  ;; object itself. The function should return a  list of expressions
  ;; (optionally null) such that will be suffixed to the computed
  ;; lambda-list, for representing each respective PARAM received by the
  ;; function.
  ;;
  ;; The OTHER-UNPARSER function, if provided, should maintain internal
  ;; state as to ensure that any appropriate lambda-list-keyword symbol
  ;; are returned. [FIXME: Untested]
  ;;
  ;; NB: OTHER-INITIALIZE, OTHER-FINALIZE

  (let* ((buf (make-array 0 :fill-pointer 0 :adjustable t))
         context
         (param (lambda-signature-first-param signature))
         expr)

    ;; NB: The lambda-list form returned by UNPARSE-LAMBDA-SIGNATURE
    ;; may not be syntactically equivalent to the original lambda list
    ;; provided to PARSE-LAMBDA-SIGNATURE.
    ;;
    ;; The original lambda list can be retrieved for purposes of
    ;; reflection, analysis, etc, via the accessor function,
    ;; LAMBDA-SIGNATURE-LAMBDA-LIST

    (labels ((add-to-buffer (elt)
               (vector-push-extend elt buf))
             (add-kwd (kwd)
               (add-to-buffer (lambda-keyword-name kwd)))
             (add-param (param)
               (add-to-buffer (param-name param)))
             (unparse-optional-param (param)
               (list (param-name param)
                     (optional-param-initform param)
                     (optional-param-value-p-name param)))
             (unparse-key-param (param)
               (let ((name (param-name param))
                     (key (key-param-keyword-name param)))
                 (list (list key name)
                       (key-param-initform param)
                       (key-param-value-p-name param))))
             (update-context (which)
               (unless (eq context which)
                 (setq context which))))

      ;; FIXME - need to handle empty KWD members, empty optional
      ;; members separate to the following ... but this would require
      ;; further update to the LAMBDA-SIGNATURE model, for so much as to
      ;; ensure that the &KEY or &OPTIONAL with-no-contextual-args
      ;; condition will be modeled from the lambda list.

      (when other-initialize
        (funcall (the function other-initialize) signature))

      (loop
         (typecase param
           (null (return))
           (required-param
            (add-param param))
           (optional-param
            (update-context (quote &optional))
            (add-to-buffer (unparse-optional-param param)))
           (rest-param
            (update-context (rest-param-kind param))
            (add-param param))
           (key-param
            ;; FIXME - need further processing here - initform etc
            ;;
            ;; FIXME - Understanding that initforms are not supported in
            ;; some lambda list syntaxes, this model should be
            ;; accompanied with forms for validating any parsed
            ;; LAMBDA-SIGNATURE onto any single lambda list syntax.
            (update-context (quote &key))
            (add-to-buffer (unparse-key-param param))
            ;; NB: &ALLOW-OTHER-KEYS is  is handled as a plain
            ;; LAMBDA-KEYWORD now, insofar as for lambda signature
            ;; unparsing. It's also stored in the KEY-SUBSET of any
            ;; LAMBDA-SIGNATURE having a non-empty set of explicit 
            ;; &KEY params
           (lambda-keyword
            (add-kwd param))
            ;; FIXME - the other-unparser support needs testing, for
            ;; this unparser function
           (t
            (when other-unparser
              (mapcar #'add-to-buffer
                      (funcall (the function other-unparser)
                               param signature)))))
         (let ((next
                (lambda-element-expr-next param)))
           (setq param next)))

      (setq expr (coerce buf 'list))
      (when other-finalize
        (funcall (the function other-finalize) expr signature))
      (values expr))))



(define-condition lambda-list-syntax-error (simple-error)
  ((lambda-list
    :initarg :lambda-list
    :reader error-lambda-list))
  (:report
   (lambda (c s)
     (apply #'format s
            (concatenate 'simple-string
                         "~<" (simple-condition-format-control c)
                         ":~>~< ~S~>")
            (append (simple-condition-format-arguments c)
                    (list (error-lambda-list c)))))))

(defmacro lambda-list-syntax-error (which fmt &rest args)
  `(error 'lambda-list-syntax-error
          :format-control ,fmt
          :format-arguments (list ,@args)
          :lambda-list ,which))

;; (lambda-list-syntax-error '(n/a) "Unsupported N/A in lambda pseudo-form")



(declaim (ftype (function
                 (list &optional
                       (or null function)
                       (or null function)
                       (or null function))
                 (values lambda-signature &optional))
                parse-lambda-signature))

(defun parse-lambda-signature (lambda-list &optional
                                               other-parser
                                               other-initialize
                                               other-finalize)
  ;;
  ;; NB: OTHER-PARSER - if provided, a function accepting three
  ;; arguments:
  ;;
  ;; - a lambda-list keyword symbol, or a symbol or list type PARAM
  ;;   expression -- namely, as not "otherwise handled", below.
  ;;
  ;; - a "context" symbol, denoting the context in which the expression
  ;;   in the first argument has been parsed
  ;;
  ;;     e.g when &aux is parsed after &rest, this second argument's
  ;;     value would be the symbol CL:&REST, while the first argument's
  ;;     value would be the symbol &AUX. Subsequently, each "aux
  ;;     parameter" would be provided as the first argument to the
  ;;     function, with the second argument's value being the symbol
  ;;     CL:&AUX
  ;;
  ;; - the original LAMBDA-LIST expression
  ;;
  ;;   - NB: This value should not be destructively modified. It may be
  ;;     assumed to represent an object of a consistent identity, onto
  ;;     EQ and SXHASH, throughout the duration of each call to
  ;;     PARSE-LAMBDA-SIGNATURE
  ;;
  ;; Should return either a PARAM object - such that this function,
  ;; PARSE-LAMBDA-SIGNATURE, will then add to the OTHER-SUSBET
  ;; internally -- or return the value NIL, indicating that no value is
  ;; to be stored for the symbol that was provided to the function's
  ;; first argument.
  ;;
  ;; NB: OTHER-INITIALIZE, OTHER-FINALIZE


  ;;
  ;; TBD: Storage for OTHER-SUBSET, and ordering when unparsing to
  ;; produce a lambda list syntactically  equivalent to the original,
  ;; unparsed list expression.
  ;;
  ;; - Concept: OTHER-SUBSET-SET as a "Meta-subset" of LAMBDA-SIGNATURE
  ;;    containing a sequence of OTHER-SUBSET ... the latter, defined
  ;;    with slots PREVIOUS NEXT => SYMBOL for purpose of ordering in
  ;;    producing the portable, "other list" representation of an
  ;;    arbitrary lambda list expression.
  ;;
  ;;   ... OR: one could just store a simple vector under an OTHER-SUBSET
  ;;   slot, and futhermore ensure the use of an OTHER-PARAM-NEXT
  ;;   slot. The "Unparsing", in either approach to storage: TBD
  ;;
  ;;  NB: at that point, the use of -NEXT slots may serve to make the
  ;;  vector storage of PARAM objects more or less redundant. However,
  ;;  this existing subset-oriented storage will be retained, generally
  ;;  for purpose of caching of operative subsets of lambda list
  ;;  parameters, excepting the genrealized storage in the OTHER-SUBSET
  ;;
  ;; see also:  UNPARSE-LAMBDA-SIGNATURE
  ;;
  ;; NB: The OTHER-PARSER function may be for adding support for &AUX,
  ;; &WHOLE, &ENV, and implementation-specific lambda list keywords -
  ;; extesionally, portably, & without any requirement for rewriting the
  ;; function, PARSE-LAMBDA-SIGNATURE.

  ;; Ed. NB: Ad hoc, but practical

  (let (first
        required-subtree
        keyword-subtree
        optional-subtree
        other-subtree
        allow-other-keys
        rest-p
        rest-param
        last
        param-last
        context
        signature)

    ;; NB: see also: LTP/COMMON:DEFUN*

    ;; FIXME: Add intrinsic VALUE-P-NAME initialization for any
    ;; INITIALIZED-PARAM without a user-provided value-p name

    (macrolet ((update-for-element (elt)
                 `(progn
                    (unless first
                      (setq first ,elt))
                    (when last
                      (setf (lambda-element-expr-next last)
                            ,elt))
                    (setq last ,elt)
                    (when (param-p ,elt)
                      (when (param-p param-last)
                        (setf (param-next param-last) ,elt))
                      (setq param-last ,elt))))
               (mk-v-p-name (name sfx)
                 `(make-symbol (concatenate 'simple-string
                                            (symbol-name ,name)
                                            ,sfx)))
               (mkerror (msg &rest args)
                 `(lambda-list-syntax-error lambda-list ,msg ,@args))
               (mk-buffer (&optional (len 0))
                 `(make-array ,len :fill-pointer ,len
                             :adjustable t))
               (subtree-p (which)
                 `(and ,which
                       (not (zerop (length (the (array t (*)) ,which))))))
               (add-to-buffer (elt which)
                 ;; NB: This model may be updated to use a read-only
                 ;; PARAM-NEXT slot. Albeit, the indirection required
                 ;; for that change may seem a little less easy to
                 ;; "Parse" from the source definition.
                 ;;
                 ;; It would, furthermore, require some changes in the
                 ;; structure of this function's definiion -- as due to
                 ;; a deferred "add to buffer" semantics, such as may be
                 ;; implemented in such update.
                 `(progn
                    ;; NB: Evaluation of either ELT or WHICH will not
                    ;; cause side-effects, for how this macro is called
                    ;; within this function.
                    (update-for-element ,elt)
                    (cond
                      (,which
                       (vector-push-extend ,elt ,which))
                      (t (setq ,which (mk-buffer 1))
                         (setf (aref ,which 0)
                               ,elt))))))

    (labels ((add-to-required-buffer (elt)
               (add-to-buffer elt required-subtree))
             (add-to-optional-buffer (elt)
               (add-to-buffer elt optional-subtree))
             (add-to-key-buffer (elt)
               (add-to-buffer elt keyword-subtree))
             (add-to-other-buffer (elt)
               (add-to-buffer elt other-subtree))
             (set-state (which)
               (cond
                 ((eq context which)
                  (mkerror "Multiple subsequent ~S keywords in lambda list"
                            which))
                 (t
                  (let ((new (make-lambda-keyword which)))
                    (update-for-element new)
                    (setq context which)))))
             (simplify (which)
               (coerce (the (array t (*)) which)
                       'simple-vector))

             (process-symbol-expr (elt)
               (declare (type symbol elt))
               (case elt
                 (&optional
                  (set-state elt)
                  (cond
                    (optional-subtree
                     (mkerror "Lambda list contains more than one &OPTIONAL symbol"))
                    (t
                     (setq optional-subtree (mk-buffer 0)))))
                 (&key
                  (set-state elt)
                  (cond
                    (keyword-subtree
                     (mkerror "Lambda list contains more than one &KEY symbol"))
                    (t
                     (setq keyword-subtree (mk-buffer 0)))))
                 (&allow-other-keys
                  ;; FIXME: If no explicit &REST parameter is specified,
                  ;; while &ALLOW-OTHER-KEYS is specified, create a
                  ;; REST-PARAM from an uninterned symbol and ensure
                  ;; that that is stored before the first KEY-PARAM in
                  ;; the parsed lambda list.
                  ;;
                  ;; Subsquently, COMPUTE-CALL-FORM may then use the
                  ;; intrinsic &REST param for maintaining call-state
                  ;; across the effective variadic call.
                  (unless keyword-subtree
                    (mkerror "Lambda list contains &ALLOW-OTHER-KEYS ~
other than after &KEY symbol"))
                  (when allow-other-keys
                    (mkerror "Lambda list contains more than one ~
&ALLOW-OTHER-KEYS symbol"))
                  (setq allow-other-keys t)
                  (set-state elt))
                 (&rest
                  ;; NB: &BODY not handled here.
                  ;;
                  ;; Handling for &BODY may be defined as to reuse the
                  ;; exixting REST-PARAM class
                  (when rest-p
                    (mkerror "Lambda list contains more than one &REST symbol"))
                  (setq rest-p t)
                  (set-state elt))
                 ((position elt other-keywords :test #'eq)
                  (process-other-expr elt))
                 (t
                  ;; ---- proceed under the current parser state ----
                  (case context
                    ((nil)
                     (add-to-required-buffer (make-required-param elt)))
                    (&optional
                     (add-to-optional-buffer
                      (make-optional-param
                       elt
                       :initform nil
                       ;; NB: No :initfunction initialized here
                       :value-p-name (mk-v-p-name elt "-p-default"))))
                    (&key
                     ;; FIXME check for &optional / warn on ambiguity
                     (add-to-key-buffer
                      (make-key-param
                       elt
                       :initform nil
                       ;; NB: No :initfunction initialized here
                       :value-p-name (mk-v-p-name elt "-p-default")
                       :keyword-name (intern (symbol-name elt)
                                             :keyword))))
                    (&rest
                     (when rest-param
                       (mkerror "Lambda list contains more than one ~
&REST parameter"))
                     (setq rest-param
                           (make-rest-param elt context))
                     (update-for-element rest-param))

                    (t (process-other-expr elt))))))

             (process-cons-expr (elt)
               (declare (type cons elt))
               (cond
                 ((eq context (quote &optional))
                  (destructuring-bind (param-name &optional
                                                  initform
                                                  (value-p nil v-p-p))
                      elt
                    (let ((initfunction
                           (when initform
                             ;; FIXME: Compile the initfunction
                             ;;
                             ;; NB: After a review of the behaviors
                             ;; of GET-SETF-EXPANSION this function
                             ;; does not implement any special
                             ;; handling for compilation environments
                             ;; in LAMBDA definitions
                             (coerce `(lambda () ,initform)
                                     'function))))

                      (unless v-p-p
                        (setq value-p (mk-v-p-name param-name
                                                   "-p-default")))

                      (add-to-optional-buffer (make-optional-param
                                               param-name
                                               :initform initform
                                               :initfunction initfunction
                                               :value-p-name value-p
                                               :value-p-p v-p-p)))))
                 ((eq context (quote &key))
                  (destructuring-bind (key-first &optional
                                                 initform
                                                 (value-p nil v-p-p))
                      elt
                    (let ((key-name
                           (etypecase key-first
                             (symbol (intern (symbol-name key-first)
                                             (lform (find-package :keyword))))
                             (cons (car key-first))))
                          (param-name
                           (etypecase key-first
                             (symbol key-first)
                             (cons (cadr key-first))))
                          (initfunction
                           (when initform
                             ;; FIXME: Compile the initfunction
                             ;;
                             ;; NB: After a review of the behaviors
                             ;; of GET-SETF-EXPANSION this function
                             ;; does not implement any special
                             ;; handling for compilation environments
                             ;; in LAMBDA definitions
                             (coerce `(lambda () ,initform)
                                     'function))))
                      (unless v-p-p
                        (setq value-p (mk-v-p-name param-name
                                                   "-p-default")))
                      (add-to-key-buffer (make-key-param
                                          param-name
                                          :keyword-name key-name
                                          :initform initform
                                          :initfunction initfunction
                                          :value-p-name value-p
                                          :value-p-p v-p-p)))))
                 (t (process-other-expr elt))))

             (process-other-expr (elt)
               (cond
                 (other-parser
                  (funcall (the function other-parser)
                           elt context lambda-list))
                 (t
                  ;; FIXME: Improve this error's  message text
                  (mkerror "Syntax not supported")))))

      (when other-initialize
        (funcall (the function other-initialize) lambda-list))

      (do-cons (elt rest lambda-list)
        (unless (listp rest)
          (mkerror "Syntax not supported - not a well formed list"))
        (typecase elt
          (symbol (process-symbol-expr elt))
          (cons (process-cons-expr elt))
          (t (process-other-expr elt))))


      ;; TBD: ELSEWHERE, emit a style-warning for a GENERIC-OP of no
      ;; specializable parameters

     (setq signature
           (%mk-lambda-signature
            :lambda-list lambda-list
            :first first
            :required (when (subtree-p required-subtree)
                        (%mk-required-subset (simplify required-subtree)))
            :optional (when (subtree-p optional-subtree)
                        (%mk-optional-subset (simplify optional-subtree)))

            ;; NB: Similar to OPTIONAL-SUBTREE, for any initial specification
            ;; of &KEY or &OPTIONAL with an empty KEY or OPTIONAL param
            ;; subset, the respective &KEY or &OPTIONAL keyword wiill be 
            ;; stored singularly in the unparse-vector of the
            ;; LAMBDA-SIGNATURE, rather than in the effective
            ;; call-form vector of the same (cf. PARAM-NEXT)
            :keyword (when (or allow-other-keys (subtree-p keyword-subtree))
                       (%mk-key-subset (simplify keyword-subtree)
                        allow-other-keys))
            :other (when (subtree-p other-subtree)
                     (%mk-other-subset (simplify other-subtree)))
            :rest rest-param))

     (when other-finalize
       (funcall (the function other-finalize) signature lambda-list))
     (values signature)
    ))))


(eval-when ()
  ;; test for a limited subset of lambda list syntax

  (defparameter *l*
    (parse-lambda-signature
     '(a b &optional q &rest other &key frob &allow-other-keys)))

  (unparse-lsignature-expr-next *L*)

  (values
   (lambda-signature-first-param *l*)
   ;; (lambda-signature-required-subset *l*)
   ;; (lambda-signature-optional-subset *l*)
   (lambda-signature-rest-param *l*)
   ;; (lambda-signature-key-subset *l*))
   )

  ;; Test the mechanical unparser
  ;;
  ;; NB: In lieu of simply retrieving the cached lambda list, this more
  ;; or less serves as something of a partial consistency check onto the
  ;; LAMBDA-SIGNATURE framework
  (unparse-lambda-signature *l*)

  (unparse-lambda-signature
   (parse-lambda-signature
    '(a b &optional q &rest other &key frob &allow-other-keys)))

  (unparse-lambda-signature
   (parse-lambda-signature
    ;; FIXME: Update parser for intrinsic value-p-name initialization
    ;; when none is provided by the user
    '(a b &optional q &rest other &key frob (tbd 12312))))

  (unparse-lambda-signature
   (parse-lambda-signature
    ;; FIXME: Update parser for intrinsic value-p-name initialization
    ;; when none is provided by the user
    '(a b &optional q &rest other &key frob ((:key tbd) 12312 key-p))))

  (lambda-signature-first-param
   (parse-lambda-signature
    ;; FIXME: Update parser for intrinsic value-p-name initialization
    ;; when none is provided by the user
    '(a b &optional q &rest other &key frob (fixme tbd))))

  ;; FIXME - empty key set was not being unparsed correctly (API updated)
  ;; (describe (parse-lambda-signature '(a b &optional q &rest other &key)))


  ;; --

  (defun unparse-lsignature-expr-next (signature)
    (let* ((first (lambda-signature-first-param signature))
           (next first)
           (bkt (make-array 0 :fill-pointer 0 :adjustable t)))
      (while next
        (vector-push-extend next bkt)
        (setq next (lambda-element-expr-next next)))
      (coerce bkt 'simple-vector)))


  (unparse-lsignature-expr-next
   (parse-lambda-signature
    '(&optional q &rest other &key))
   )

   (defun unparse-lsignature-param-next (signature)
    (let* ((first (lambda-signature-first-param signature))
           (next first)
           (bkt (make-array 0 :fill-pointer 0 :adjustable t)))
      (while next
        ;; NB: this assumes that FIRST is a PARAM
        (vector-push-extend next bkt)
        (setq next (param-next next)))
      (coerce bkt 'list)))

   ;; some tests for the PARAM-NEXT handling in PARSE-LAMBDA-SIGNATURE

   (unparse-lsignature-param-next
    (parse-lambda-signature
     '(a &optional q &rest other &key))
    )
   ;; => (<<REQUIRED-PARAM A>> <<OPTIONAL-PARAM Q>> <<REST-PARAM other>>)

   (param-next (car
                (unparse-lsignature-param-next
                 (parse-lambda-signature
                  '(a &optional q &rest other &key)))))
   ;; => <<OPTIONL-PARAM Q>>

   ;; -- test for parsing of empty param subsets

   (defparameter *l2*
     (parse-lambda-signature
      '(a &optional &rest other &key)))

   (lambda-signature-optional-subset *l2*)
   ;; => NIL

   (lambda-signature-key-subset *l2*)
   ;; => NIL

   (unparse-lambda-signature *l2*)
   ;; => <equivalent lambda list>

  ;; -- test some failure cases
  (parse-lambda-signature '(&optional q &optional dnw &rest other &key))
  (parse-lambda-signature '(&key q &key dnw &rest other &key))
  (parse-lambda-signature '(&optional q &key dnw &optional dnw-2))
  (parse-lambda-signature '(&key q &optional dnw &key dnw-2))

  (parse-lambda-signature '(&key q &optional dnw &allow-other-keys))
  ;; ^ FIXME should err



)



;; -- Partial MOP Interop

(defclass generic-callable (funcallable-standard-object)
  ())

(defclass generic-op (generic-callable #+NIL standard-generic-class)
  ;; NB: See remarks, below, concerning GENERIC-METHOD
  ((name
    :reader generic-op-name
    ;; :access (:read :write-once)
    :type symbol)
   (signature
    :reader generic-op-signature
    ;; :access (:read :write-once)
    :type lambda-signature)
   (methods
    ;; :type simple-vector
    ;; :access (:read :write)
    :accessor %generic-op-methods)
   (cached-methods
    ;; NB: This would include any set of methods defined for purpose of
    ;; caching for effective method call-forms, for parameter sets to
    ;; which no single, directly specialized method would apply.
    ;;
    ;; NB: Similar to memoization in MOP, this set of cached methods may
    ;; be -- as a set -- made effectively invalid when any GENERIC-METHOD
    ;; is added to or removed to the GENERIC-OP.
    ;;
    ;; :type simple-vector
    ;; :access (:read :write)
    :accessor %generic-op-cached-methods)
   #+TBD
   (specialized-params
    ;; NB: Towards providing for definitions of a GENERIC-OP function
    ;; such that any GENERIC-METHOD defined to the GENERIC-OP would be
    ;; specialized onto a subset of required parameters to the GENERIC-OP
    ;; ... with a semantics not otherwise differing from a GENERIC-OP
    ;; such that would be specialized on every required parameter to the
    ;; GENERIC-OP
    ;;
    ;; NB: This protocol may define it as an error, to define a
    ;; GENERIC-OP without at least one specializable parameter.
    ;; :type simple-vector
    :reader %generic-op-specialized-params)
   )
  ;; It reuses this from CLOS, NB:
  (:metaclass funcallable-standard-class))


(declaim (ftype (function (generic-op)
                          (values cons &optional))
                generic-op-lambda-list))


(defun generic-op-lambda-list (genop)
  ;; NB: This returns a cached lambda-list, such that may not be
  ;; completely representative of every lambda list used in any single
  ;; GENERIC-OP call form.
  ;;
  ;; See also: Other annotations, in this source file.
  (cond
    ((slot-boundp genop 'signature)
     (the cons (lambda-signature-lambda-list
                (generic-op-signature genop))))
    (t
     (error "No lambda-signature initialized for ~S" genop))))


(defclass generic-method (generic-callable #+nil standard-method)
  ;; NB: Defininig this as a subclass of STANDARD-METHOD would be, in
  ;; short terms, "A stretch." However it may serve to emulate some
  ;; generic qualities of the STANDARD-METHOD class, and may whatsoever
  ;; emulate any quality of MOP in any application, but considering the
  ;; liberties that this protocol may be said to take with regards to
  ;; standard MOP conventions, this class should not actually be defined
  ;; as a subclass of STANDARD-METHOD, here.
  ()
  (:metaclass funcallable-standard-class))


(defgeneric generic-op-method-class (generic-op)
  (:method ((genop generic-op))
    (declare (ignore genop))
    ;; structurally similar to GENERIC-FUNCTION-METHOD-CLASS
    ;;
    ;; NB: This standard-method does not use any accessor onto the GENOP
    (find-class 'generic-method)))


;; ---



(defgeneric compute-ftype-params-type (genop)
  #+FIXME
  (:method ((genop generic-op))
    (let ((signature (generic-function-lambda-signature genop)))
      ;; see also: defun* - concerning partially derived FTYPE procalmations
      ;;
      ;; Note the function PARSE-LAMBDA-SIGNATURE. defined above
      ;;
      ;; - would not be needed here, directly, for a GENERIC-OP
      ;;   in which (GENERIC-OP-SIGNATURE GENOP)
      ;;   is assumed to return a LAMBDA-SIGNATURE
      ;;
      ;; - may be of some use if implementing a method for this
      ;;   function, specialized directly onto STANDARD-GENERIC-CLASSa
      ;;
      ;; - NB handle the LAMBDA-SIGNATURE-REQUIRED-SUBSET mainly,
      ;;   with special processing as per e.g methods defined to the
      ;;   GENOP at the time when this function is called.
      ;;
      ;; - TBD Provide some manner of caching and update mechanism, such
      ;;   that may serve to ensure that any new ftype declration has
      ;;   all specializable parameters specified as subtypep their
      ;;   specification in any previous declaration. Failing that,
      ;;   consider using CERROR as to permit any incompatible ftype
      ;;   declaration, after notification & #"CONTINUE
      ;;
      ;; - FIXME also enure that this is called in the compiler
      ;;   environment, when defining slot accessors onto an extension of
      ;;   STANDARD-GENERIC-FUNCTION - to which, the set of definitive
      ;;   ftype declarations may be cached and emitted at some point
      ;;   after the top-level ENSURE-CLASS call. See also, the
      ;;   SINGLETON definition in LTP. NB: DEFCLASS-LIKE-DEFSTRUCT
      )))

(defgeneric compute-ftype-values-type (genop)
  (:method ((genop generic-op))
    `(values t)))

(defgeneric compute-ftype (genop)
  (:method ((genop generic-op))
    `(ftype (function ,(compute-ftype-params-type genop)
                      ,(compute-ftype-values-type genop))
            ,(generic-function-name genop))))


;; ---

#|
  Topic: Initialization of Instance-Specialized Callback Functions,
         via Methods Generalized on Class of Instance


  - Usage Case: COMPUTE-CALL-FORM for runtime evaluation of function
    calls to a GENERIC-OP

    - TBD: Generalized interoperability onto MOP conventions.

      - COMPUTE-EFFECTIVE-METHOD

      - MAKE-METHOD-LAMBDA

      - CALL-METHOD

      - Note the usage of an intrinsic CONS, throughout MOP, for
        representing the arguments to the original generic function
        call. Although this may be considered as a generalization,
        roughly, onto variadic &REST semantics and although the original
        "Argument list" -- as produced, one assumes, from the initial
        function call -- may be reused throughout the numerous method
        calls of the veritable "Standard MOP machine," but is this truly
        an optimal methodology? requiring that a new CONS will be
        produced, for every call to a generic function? and rather,
        using two CONS - one for the initial sequence of arguments to
        the original function call, and one that would be produced
        within MOP, as an ephemeral container for methods computed
        dynamically, for the list-based method dispatching protocol?

|#


#+nil
(defun frob-call (a b &key (c nil c-p) (d nil d-p))
  (labels ((frob-call-internal (a b &key
                                  (c nil c-p)
                                  (d nil d-p))
             (warn "FROB-CALL called with ~S ~S (~S ~S) (~S ~S)"
                   a b c c-p d d-p)
             ))
    (apply #'frob-call-internal
           ;; TBD: CONS usage in implementation of APPLY (??)
           a b
           (nconc (when c-p (list :c c))
                  (when d-p (list :d d)))
           )))

;; (frob-call 1 2 :c 3 :d 4)

;; (frob-call 1 2 :d 4)

;; (frob-call 1 2 :c nil)

(declaim (ftype (function (lambda-signature)
                          (values boolean &optional))
                call-form-variadic-p))


;; NB: Call-Wrapping for GENERIC-OP and GENERIC-METHOD
;;
;; TBD: GENERIC-METHOD Specialization, static GENERIC-METHOD function
;;      definitions, static GENERIC-OP function definitions -- simply,
;;      in a manner of CLOS-oriented "Call wrapping," with support for a
;;      lexically scoped CALL-SUPER function, not in all ways like
;;      CALL-NEXT-METHOD -- and binidings onto FUNCALLABLE-INSTANCE
;;
;; In effect, it should be possible for the method function of any
;;  defined GENERIC-METHOD to be called directly, as a function with a
;; lambda list compatible to that of the generic function to which the
;; method is defined.


(defun call-form-variadic-p (signature)
  ;; FIXME only used once, so far
  (macrolet ((init-p (whence)
               `(and (,whence signature) t)))
    (or (init-p lambda-signature-optional-subset)
        (init-p lambda-signature-key-subset)
        #+TBD (init-p lambda-signature-other-subset)
        )
    ;; NB also VARIADIC-P when null &KEY subset but &ALLOW-OTHER-KEYS
    ;;
    ;; NB: VARIADIC-P when any REST-PARAM, in precedence to any KEY
    ;; subset or &KEY with &ALLOW-OTHER-KEYS
    ))



(defgeneric unparse-param-for-call (param whence)
  ;; NB: Extensions with regards to OTHER-SUBSET of a LAMBDA-SIGNATURE
  ;; should define methods onto this function, or errors may result
  ;; during UNPARSE-FOR-CALL
  (:method ((param required-param) (whence generic-callable))
    (declare (ignore whence))
    (required-param-name param))
  ;; NB: Effectively variadic params must return a list
  (:method ((param optional-param) (whence generic-callable))
    ;; NB: Used directly for OPTIONAL-PARAM,
    ;;     and used via "dispatch to super" call for KEY-PARAM
    (declare (ignore whence))
    (let ((param-name (optional-param-name param)))
      ;; TBD: Assuming that the resulting call form will be evaluated
      ;; within an environment in which the containing lambda list
      ;; will have served to establish any initial values,
      ;; consdierations:
      ;;
      ;; - The initfunction semantics of INITIALIZED-PARAM may be
      ;;   removed from this design

      ;; Assumption: This form will be evaluated within NCONC
      ;;
      ;; Same assumption applies to the KEY-PARAM method, defined below.
      `(when ,(optional-param-value-p-name param)
         (list ,param-name))))

  (:method ((param key-param) (whence generic-callable))
    (declare (ignore whence))
    (let ((param-name (key-param-name param)))
      `(when ,(key-param-value-p-name param)
         (list ,(key-param-keyword-name param)
               ,param-name))))
  )


(defun unparse-signature-for-call (whence signature)
  (declare ;; (ignorable whence)  ;; NB: May be removed in a subsq. revision
           (type lambda-signature signature)
           #+SBCL (values list list &optional))

  ;; refer to remarks in COMPUTE-CALL-FORM
  ;;
  ;; .. esp. concerning CALL-SUPER semantics for GENERIC-OP and
  ;;    GENERIC-METHOD definitions
  ;;
  (macrolet ((mkv ()
               (make-array 0 :fill-pointer 0 :adjustable t)))
    (let* ((required-sub (lambda-signature-required-subset signature))
           (optional-sub (lambda-signature-optional-subset signature))
           (key-sub (lambda-signature-key-subset signature))
           ;; NB: &REST param unused in constructing the call-form

           ;; TBD: unparse-signature-for-call for other-subset
           ;; Policy: Storing results in VARIADIC-FORMS
           (other-sub (lambda-signature-other-subset signature))

           (variadic-p  (call-form-variadic-p signature))
           (required-args (mkv))
           (variadic-forms (when variadic-p (mkv))))
      ;; NB: within VARIADIC-FORMS:
      ;; - store any required conditional function calls for initializers
      ;;   of &OPTIONAL and &KEY parameters


      ;; FIXME - iterate simply across PARAM-NEXT, below (??)

      (when required-sub
        ;; parameters onto a call for a form of known arity
        (do-vector (param (required-subset-members required-sub))
          (vector-push-extend (unparse-param-for-call param whence)
                              required-args)))
      ;; parameters and parameter initializers onto variadic calls
      (when optional-sub
        (do-vector (param (optional-subset-members optional-sub))
          (let ((result (unparse-param-for-call param whence)))
            (vector-push-extend result variadic-forms))))
      (when key-sub
        (do-vector (param (key-subset-members key-sub))
          (let ((result (unparse-param-for-call param whence)))
            (vector-push-extend result variadic-forms))))
      (when other-sub
        (do-vector (param (other-subset-members other-sub))
          (let ((result (unparse-param-for-call param whence)))
            (vector-push-extend result variadic-forms))))

      (values (coerce (the (array t (*)) required-args) 'list)
              (when variadic-p
                (coerce (the (array t (*)) variadic-forms) 'list)))
      )))


(defun compute-call-form (whence lambda-signature)

  ;; FIXME: Still need to support:
  ;;
  ;; - Selection of GENERIC-METHOD instnces from a GENERIC-OP, as based
  ;;   on classes of required parameter sprovided in a call to the
  ;;   GENERIC-OP - note that this does not, in fact, require creation
  ;;   of arbitrary CONS objects for those parameters. It may dispatch
  ;;   at least partly on the LAMBDA-SIGNATURE-REQUIRED-SUBSET of the
  ;;   LAMBDA-SIGNATURE defined to a GENERIC-OP
  ;;
  ;; - Caching of effective call forms for GENERIC-METHOD calls to which
  ;;   no single GENERIC-METHOD has an in-all-EQ set of method
  ;;   specializers (may not be uncommon, in applications)
  ;;
  ;; - Dispatching across GENERIC-METHOD call forms, and the definition
  ;;   and usage of the function CALL-SUPER in GENERIC-METHOD lambda
  ;;   forms.
  ;;
  ;; - Updating for dispatch-chains when a GENERIC-METHOD is added to or
  ;;   removed from a GENERIC-OP - in all regards, using a static
  ;;   dispatching.
  ;;
  ;; - Handling for specific protocol constraints, to include:
  ;;   - A GENERIC-OP must be defined with at least one specializable
  ;;     parameter
  ;;   - A GENERIC-OP may be defined with a specilized FTYPE. However,
  ;;     on addition of any GENERIC-METHOD to such a GENERIC-OP, the
  ;;     added method must be verified such as to not conflict with
  ;;     the existing FTYPE form. Failing that, for purposes of
  ;;     interactive development, an error may be signaled with restart
  ;;     vis CERROR - somewhat analogous to some behaviors with regards
  ;;     to DEFSTRUCT.
  ;;
  ;; - GENERIC-OP usage for slot-value and slot-boundp accessors -
  ;;   partly onto MOP.
  ;;
  ;; - Extensions to this protocol, e.g for defining a storage and
  ;;   call-form protocol for class-owned methods and corresponding
  ;;   funcallable instance forms.
  ;;
  ;; - TBD: Analogy onto CLOS/MOP :AROUND, :BEFORE, and :AFTER methods,
  ;;   as with standard method combination in CLOS

  ;; Prototype for an analogy to both of COMPUTE-DISCRIMINATING-FUNCTION
  ;; and MAKE-METHOD-LAMBDA onto GENERIC-OP and GENERIC-METHOD

  ;; NB: Towards application for GENERIC-OP specialization onto
  ;;     instances of GENERIC-METHOD, this functional prototype
  ;;     should be applied as to ensure that the first, most
  ;;     specialized, statically available method for each specializable
  ;;     paramter in LAMBDA-SIGNATURE will be selected and called for
  ;;     the appropriate call-form - as namely a GENERIC-METHOD
  ;;     function, serving in a role of WHENCE.
  ;;
  ;;     Method-local dispatching within those methods may be handled
  ;;     via a generic CALL-SUPER function - not in all ways equivalent
  ;;     to CALL-NEXT-METHOD, though providing a similar semnatics as to
  ;;     how CALL-SUPER may be called (assuming no mangling of the
  ;;     arguments to the initial call-form, or however otherwise).
  ;;
  ;;     Method-function resolution for CALL-SUPER may be addressed for
  ;;     each GENERIC-METHOD added to a GENERIC-OP, after the
  ;;     addition of any new GENERIC-METHOD to a GENERIC-OP - as to
  ;;     suport an in-most-ways staic dispatching onto method
  ;;     forms. This assumes that every class used as a specializer to a
  ;;     GENERIC-OP will have been defined (not as a forward-referenced
  ;;     class) and finalized at the time when any method applying that
  ;;     class, as a specializer, will be initialized and stored as a
  ;;     GENERIC-METHOD onto a GENERIC-OP. This also assumes that
  ;;     GENERIC-OP will support only class specializers in any
  ;;     GENERIC-METHOD.
  ;;
  ;;     NB: As a side-effect to this approach for static method
  ;;     dispatching, the method call infrastructure may need not
  ;;     utilize a lot of epehmeral CONS objects.

  ;; NB: Not wrapping the call-form in LAMBDA here, during prototyping
  (let ((variadic-p (call-form-variadic-p lambda-signature)))
       ;; NB: WHENCE may represent a GENERIC-METHOD-FUNCTION of a
       ;; GENERIC-METHOD - NB: it's assumed to be an object, not an
       ;; evaluable expression. TBD: DEFINE-GENERIC-MACRO as such.
       ;;
       #+NIL
       (declare (type function ,%whence)
                (dynamic-extent ,%whence))

       ;; NB: Intrinsic CONS production for variadic mapping of APPLY
       ;;     onto FUNCALL - required here, if only when the
       ;;     LAMBDA-SIGNATURE contains either &OPTIONAL or &KEY forms,
       ;;     and thus for the characteristics of ephemeral &OPTIONAL
       ;;     and &KEY parameters, their initializers, and their
       ;;     "value-p" parameters, in some instances of conventional
       ;;     Common Lisp function calls.
       ;;
       ;;     Notable as a concern for applications: The extent to which
       ;;     the GC must be used, for so much as normal call routines,
       ;;     in a Common Lisp implementation - notable, in all of the
       ;;     lazy memory allocation apparently assumed to be necessary
       ;;     for the implementation of this programming language.
       ;;
       ;;     APPLY may considered convenient, but estimably wasteful.

       ;; FIXME: Only use APPLY here, if there are keyword or optional
       ;; parameters, in the parsed LAMBDA-SIGNATURE


       ;; FIXME: In a further refinement of the design of the GENERIC-OP
       ;; and GENERIC-METHOD protocol defined here, this protocol may
       ;; prevent appliation of KEY, OPTIONAL, and REST parameters
       ;; insofar as onto method specialization - thus further
       ;; simplifying the design and implementation of this protocol, at
       ;; no estimably impossible cost to subsequent applications.
       ;;
       ;; For now, simply ensuring that the distriminating function will
       ;; not use APPLY if the GENERIC-OP does not permit any KEY or
       ;; OPTIONAL parameters ... provides a local optimization, such
       ;; that may be furthermore denoted in systems documentation.
       ;;
       ;; Towards a further optimization, as though by side effect: Note
       ;; also, the limitation of not permitting EQL specializers in
       ;; GENERIC-OP and GENERIC-METHOD definitions.

       ;; NB: for any KEY or OPTIONAL param, ensure that the
       ;; initfunction for the param is called when no actual value is
       ;; provided - assuming any non-nil initfunction, for each.
       ;; ^ FIXME: Also ensure that thoes initfunctions are compiled,
       ;;   when initialized

       ;; TBD: Wrap the following in constant-p LOAD-TIME-VALUE (??)
       `(,@(if variadic-p
               ;; ASSUMPTION: WHENCE is, in all calls to this function,
               ;; a GENERIC-CALLABLE
               `((function apply) (the function ,whence))
               `((function funcall) (the function ,whence)))

           ;; FIXME - MV-RET & when VARIADIC-P, wrap second value in NCONC
          ,@(multiple-value-bind (req-p varargs)
                (unparse-signature-for-call whence lambda-signature)
              ;; FIXME/TBD: OTHER-SUBSET support & VARIARIC-P in impl
              (cond
                (variadic-p
                 `(,@req-p (nconc ,@varargs)))
                (t `(,@req-p))))

           ;; FIXME when &ALLOW-OTHER-KEYS was specified in the initial
           ;; lambda list, a &REST param should also have been specified
           ;; or should be added by PARSE-LAMBDA-SIGNATURE (uninterned
           ;; symbol) and should be unparsed here.
           ;;
           ;; TBD: With such a lambda signature, consider whether
           ;; anything past the REST-PARAM need be provided in the call form

          ;; TD: Storing - together - any set of OPTIONAL and KEY
          ;; params from the SIGNATURE for a list type final arg to
          ;; APPLY

          ;; ...
          ;; NB: WHENCE may be ignorable in UNPARSE-SIGNATURE-FOR-CALL, assuming
          ;; the latter would be defined as a conventional DEFUN rather
          ;; than defined as generalized onto classes of WHENCE. In an
          ;; abstrct regard, it provides for a pairing of a
          ;; LAMBDA-SIGNATURE to an actual "Call instance" (e.g a
          ;; GENERIC-METHOD)
          ;;
          ;; Starting at LAMBDA-SIGNATURE-FIRST
          ;; - unparse required parameters for call, from LSIGNATURE
          ;;
          ;; - unparse optional and keyword parameters for call, from
          ;;   LSIGNATURE
          ;;
          ;; - skipping any REST-PARAM, as its value would be
          ;;   initialized only for the called function, i.e WHENCE
          ;;
          ;; - ... and handling any OTHER-SUBSET of the LSIGNATURE
          ;;   - TBD: OTHER-SUBSET-CALL-APPLY-P
          ;;   - NB: Skip any AUX param, for reasons similar to why
          ;;         the REST-PARAM is skipped in producing the call
          ;;         form
          ;;   - NB: Skip any ENV param, similarly.
          ;;   - ... and handling any implementation-specific lambda
          ;;     list parameter kinds, assuming support for any more
          ;;     specialized lambda list syntax onto this protocol
          ;; ...
          ;; ...... assuming "Appropriate binding of the parameters,"
          ;; generically, in the environment in which this
          ;; macroexpansion would be evaluated, such as within a LAMBDA
          ;; form of a lambda signature per LAMBDA-SIGNATURE, or in any
          ;; binding of such a lambda form onto a funcallable instance.
          ;;
           )))


;; TBD: Systems Test Forms - COMPUTE-CALL-FORM


#+FROB ;; NB DNW for applications: Specialized CONS-type function names.
(defun (frob a b c) ()
  (values t t))

#+TBD ;; NB: may be analogous to MOP  COMPUTE-DISCRIMINATING-FUNCTION
(defun compute-generic-call-form (genop)
  (let ((signature (generic-op-signature genop)))
    `(lambda ,(lambda-signature-lambda-list signature)
       ,(compute-call-form op genop))))

(eval-when ()

 (defparameter *c1*
   (make-instance 'generic-op))

 ;; (functionp *c1*)

  (compute-call-form
   *c1*
   (parse-lambda-signature
    '(a b &optional q &rest other &key frob &allow-other-keys)))

)

;; ---

;; NB Design Decision: Definining a GENERIC-OP with no specilizable
;; parameters will result in a ERROR

(declaim (ftype (function (generic-op cons)
                          (values cons &optional))))
;; NB: => CONS-OF GENERIC-METHOD

#+TBD_IMPLEMENT
(defun compute-reachble-methods (genop specializers)
  ;; NB This is not, per se: MOP COMPUTE-EFFECTIVE-METHOD

  ;; NB: The list returned from this function should only be used when
  ;; a GENERIC-METHOD is added to or removed from a GENERIC-OP.
  ;;
  ;; The GENERIC-OP framework is defined as to use a methodology of
  ;; static method dispatching.
  ;;
  ;; A GENERIC-OP may be specialized only onto any one or more
  ;; classes, such that - as with generic functions and standard methods
  ;; in CLOS - must be finalized when a GENERIC-METHOD is defined to a
  ;; GENERIC-OP. The convention of a default specializer class T is
  ;; retained, vis a vis CLOS. EQL specializers are not supported.
  ;;
  ;; Specialization of a GENERIC-OP may be permitted for a subset
  ;; of the required parameters of a GENERIC-OP.
  ;;
  ;; Specialization will be supported in a manner similar to
  ;; specialization with a standard method combination in CLOS.

  ;; Thus, SPECIALIZERS: a CONS, each element of which is a CLASS

  ;; NB: If this function is to return a CONS, it must err for when no
  ;; GENERIC-METHOD is reachable for any call form, provided the set of
  ;; SPECIALIZERS for arguments to the call form
  ;;
  ;; This function should not be called until after at least one
  ;; GENERIC-METHOD is in, in effect, defined to -- in another term,
  ;; registered to -- a GENERIC-OP,
  ;;
  ;; The definition of DEFINE-GENERIC-OP should be approached in a
  ;; manner such that COMPUTE-REACHABLE-METHODS will be called for each
  ;; set of specializers in each method defined in the DEFINE-GENERIC-OP
  ;; call, but not until after the entire set of methods has been
  ;; registered to the GENERIC-OP.
  ;;
  ;; --
  ;;
  ;; For any GENERIC-METOHD that does not call CALL-SUPER in the method
  ;; lambda form, then any methods that would be reachable for any
  ;; CALL-SUPER call -- if it was present in that method's lambda form
  ;; -- may  be discarded for the purpose of  initializing the
  ;; funcallable instance function for the respective method.
  ;;
  ;; The funcallable instance function for a GENERIC-OP should be
  ;; defined sa to select the first reachable method for the set of
  ;; parameters provided in the call to the GENERIC-OP, then to
  ;; immediately dispatch to that method's funcallable instance
  ;; function, providing the same parameters to that function such that
  ;; were provided in the call to the GENERIC-OP.
  ;;
  ;; MOP has its conveniences, even in so much generalization onto
  ;; &REST. Although the amount of consuing used for method calculations
  ;; in MOP may not seem to be prohbitive to applications, but insofar
  ;; as that a CLOS-like method dispatching system may be implemented
  ;; without construction of so many ephemeral list objects at the time
  ;; of a call to function, it may be desired that so much consing would
  ;; be avoided, in applications. Although a protocol avoiding such
  ;; methodology may seem anyhow unwieldy, in implementation, but if
  ;; such a protocol may be implemented effectively, portably, and to no
  ;; forseeable error for applications, perhaps it may be of some use in
  ;; any subsequent development.

  )

;; ---

#+NIL
(defmacro define-generic-op (name signature ....))


#+NIL
(defmacro define-op-method (name signature &body body &environment env))


#+NIL
(defmethod compute-discriminating-function ((gf generic-op))
  (let ((llist (generic-function-lambda-list gf)))
    (compile (generic-function-name gf)
             `(lambda ,llist
                ;; ...
                ))))
