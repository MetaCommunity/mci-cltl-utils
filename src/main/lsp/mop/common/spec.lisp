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

(defmacro do-cons ((first rest whence &optional return) &body body)
  ;; FIXME - move to ltp/common
  (with-symbols (dispatch %whence)
    `(block nil
       (labels ((,dispatch (,%whence)
                  ;; NB: RETURN is referenced twice, below.
                  ;;
                  ;; The expanded value of RETURN will only be evaluated
                  ;; once.
                  (cond
                    ((consp ,%whence)
                     (let ((,first (car ,%whence))
                           (,rest (cdr ,%whence)))
                       ,@body
                       (cond
                         ((consp ,rest) (,dispatch ,rest))
                         (t (return ,return)))))
                    ((null ,%whence) (return ,return))
                    (t (error 'type-error
                              :expected-type 'list
                              :datum ,%whence
                              #+SBCL :context #+SBCL "as provided to DO-CONS"))
                    )))
         (,dispatch ,whence)))))

(eval-when ()

  (do-cons (a rest (class-precedence-list (find-class 'string))
              (values nil nil))
    (format t "~%~S" a))


  (do-cons (a rest '(a b . c)
              (values nil nil))
    (format t "~%~S : ~S" a rest))

  (do-cons (a b '(a . b) (values nil nil))
    (format t "~%~S : ~S" a b))

  (do-cons (a b '(a) (values nil nil))
    (format t "~%~S : ~S" a b))

  (do-cons (a b nil (values nil nil))
    (format t "~%~S : ~S" a b))


  (do-cons (a b 'arbitrary (values nil nil))
    (format t "~%~S : ~S" a b))


)

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
   :read-only t))


(defstruct (lambda-keyword
             (:include lambda-element)
             (:constructor
              make-lambda-keyword (name))))

(defstruct (param
             (:include lambda-element)
             (:constructor))
  (next
   nil ;; indicating, in effect, "End of lambda list"
   :type (or param null)
   ;; NB: Theoretically, COMPUTE-LAMBDA-SIGNATURE could be updated to
   ;; allow for this slot to be defined as not read-only. It's been
   ;; defined as being not read-only, due to how PARAM subclases
   ;; will be initialized sequentially, within
   ;; COMPUTE-LAMBDA-SIGNATURE.
   ;l
   ;; As to why this slot is not defined as read-only, remarks in
   ;; the source form of COMPUTE-LAMBDA-SIGNATURE may serve to detail
   ;; that decision.
   ;;
   ;; If any application destructively modifies the value of this slot,
   ;; in any procedure external to COMPUTE-LAMBDA-SIGNATURE, the
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

(defstruct (required-param-subset
             (:include param-subset)
             (:constructor %mk-required-param-subset
                           (members))))


(defstruct (optional-param
             (:include initialized-param)
             (:constructor make-optional-param
                           (name
                            &key
                            ;; NB: Assuming the environment for the
                            ;; initfunction is "Just handled," portably,
                            ;; in applications. also for KEYWORD-PARAM
                            initform
                            initfunction
                            value-p-name
                            value-p-p))))

(defstruct (optional-param-subset
             (:include param-subset)
             (:constructor %mk-optional-param-subset
                           (members))))


(defstruct (rest-param
             (:include param)
             (:constructor make-rest-param
                           (name &optional (kind (quote &rest)))))
  ;; FIXME add parser/unparser support for this
  (kind
   (quote &rest)
   :type symbol ;; NB narrower type (member &rest &body) ; typically
   :read-only t))


(defstruct (keyword-param
             (:include initialized-param)
             (:constructor make-keyword-param
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
   :read-only t)
  ;;
  ;; TBD subsq. usage for modelingof e.g defun/anononymous lambda and
  ;; method lambda lists
  ;;
  ;; TBD: Define an initializer (constructor wrapper) function deriving
  ;;      a defult keyword-name from the param-name
  ;;
  ;; Consider definiing methods:
  ;; - SERIALIZE-FOR (KEYWORD-PARAM GENERIC-OP)
  ;; - SERIALIZE-FOR (KEYWORD-PARAM GENERIC-OP-METHOD)
  ;; - SERIALIZE-FOR (KEYWORD-PARAM FUNCTION)
  ;; - SERIALIZE-FOR (KEYWORD-PARAM GENERIC-MACRO) ;; tbd
  ;; - SERIALIZE-FOR (KEYWORD-PARAM GENERIC-TYPE) ;; tbd
  ;; ...
  ;;
  ;; if not also a function
  ;; MAKE-KEYWORD-PARAM* (arg-form)
  ;; providing something of reusable parser interface
  ;; ... and behaviors denoted above
  )


(defstruct (keyword-param-subset
             (:include param-subset)
             (:constructor %mk-keyword-param-subset
                           (members &optional allow-other-keys))
             )
  (allow-other-keys
   nil
   :type boolean
   :read-only t))


#+NIL ;; unused as yet - see annotations, subsq.
(defstruct (aux-param
             (:include initialized-param)
             (:constructor make-aux-param (name #:FIXME_INIT))))


(defstruct (other-param-subset
             (:include param-subset)
             (:constructor %mk-other-param-subset
                           (members))))


(defstruct (lambda-signature
             (:constructor %mk-lambda-signature
                           (&key
                            lambda-list
                            ((:first first-param) nil)
                            ((:required required-param-subset) nil)
                            ((:optional optional-param-subset) nil)
                            ((:keyword keyword-param-subset) nil)
                            ((:other other-subset) nil)
                            ((:rest rest-param) nil)
                            )))
  ;; Ed. NB: See remarks about the design of this lambda list parser
  ;; model, in the COMPUTE-LAMBDA-SIGNATURE source form

  (first-param
   nil
   :type param
   :read-only t)

  (lambda-list
   ;; NB: Redundant storage, but usable for PRINT-OBJECT methods
   (lform (list (make-symbol "Unbound Lambda List")))
   :type list
   :read-only t)

  (required-param-subset
   nil
   :type (or required-param-subset null)
   :read-only t)
  (optional-param-subset
   nil
   :type (or optional-param-subset null)
   :read-only t)
  (keyword-param-subset
   nil
   :type (or keyword-param-subset null)
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
   :type (or other-param-subset null)
   :read-only t)
  ;; TBD in extensions:
  ;; - AUX-PARAM-SUBSET - specifically for anonymous lambdas & defun forms
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
  ;; functions, COMPUTE-LAMBDA-SIGNATURE and COMPUTE-LAMBDA-LIST, this
  ;; parser model now provides a methodology for extending the existing
  ;; support to allow for other lambda list syntaxes - assuming,
  ;; specialized extensions of the PARAM class, correspondingly, and
  ;; some specific behaviors in the OTHER-HANDLER functions for each of
  ;; those call forms.
  )


(defmethod print-object ((object lambda-signature) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (princ (lambda-signature-lambda-list object)
           stream)
    ))


;; (declaim (ftype (function (sequence &optional t)
;;                           (values lambda-signature &optional))
;;                 make-lambda-signature))
;;
;; FIXME Unreachable for LAMBDA-SIGNATURE v2. See COMPUTE-LAMBDA-SIGNATURE
;;
;; (defun make-lambda-signature (params &optional allow-other-keys)
;;   (%mk-lambda-signature (coerce params 'simple-vector)
;;                           (and allow-other-keys t)))


;; ----


(declaim (ftype (function (lambda-signature
                           #+TBD &optionl
                           #+TBD function)
                          (values list &optional))
                compute-lambda-list))


;; FIXME/TBD: COMPUTE-CALL-FORM cf. GENERIC-OP

(defun compute-lambda-list (signature &optional other-handler)

  ;; FIXME: Needs further update
  ;;
  ;; - Empty &KEY and &OPTIONAL subsets not being correctly unparsed.
  ;;
  ;;   - The present LAMBDA-SIGNATURE definition may need to be updated
  ;;     for those special conditions of syntax

  ;; NB: OTHER-HANDLER - if provided, a function accepting two
  ;; argument, i.e a PARAM object not handled below, and the SIGNATURE
  ;; object itself. The function should return a  list of expressions
  ;; (optionally null) such that will be suffixed to the computed
  ;; lambda-list, for representing each respective PARAM received by the
  ;; function.
  ;;
  ;; The OTHER-HANDLER function, if provided, should maintain internal
  ;; state as to ensure that any appropriate lambda-list-keyword symbol
  ;; are returned. [FIXME: Untested]
  ;;
  ;; NB: The OTHER-HANDLER should accept two arguments, namely
  ;; the "Other" PARAM and the original LAMBDA-SIGNATURE

  (let* ((buf (make-array 0 :fill-pointer 0 :adjustable t))
         context
         (kwd (lambda-signature-keyword-param-subset signature))
         (kwd-last (when kwd
                     (let* ((kwd-memb (keyword-param-subset-members kwd))
                            (n (length (the simple-vector kwd-memb))))
                       (when (plusp (the array-index n))
                         (svref kwd-memb (1- n))))))
         (kwd-others-p (when kwd
                         (keyword-param-subset-allow-other-keys kwd)))
         (param (lambda-signature-first-param signature)))
    (labels ((add-to-buffer (elt)
               (vector-push-extend elt buf))
             (add-param (param)
               ;; FIXME need to do further type dispatching here,
               ;; or in the loop below
               (add-to-buffer (param-name param)))
             (update-context (which)
               (unless (eq context which)
                 (setq context which)
                 (add-to-buffer which))))

      ;; FIXME - need to handle empty KWD members, empty optional
      ;; members separate to the following ... but this would require
      ;; further update to the LAMBDA-SIGNATURE model, for so much as to
      ;; ensure that the &KEY or &OPTIONAL with-no-contextual-args
      ;; condition will be modeled from the lambda list.

      (loop
         (typecase param
           (null (return))
           (required-param
            (add-param param))
           (optional-param
            (update-context (quote &optional))
            (add-param param))
           (rest-param
            (update-context (quote  &rest))
            (add-param param))
           (keyword-param
            ;; FIXME - need further processing here - initform etc
            ;;
            ;; FIXME - Understanding that initforms are not supported in
            ;; some lambda list syntaxes, this model should be
            ;; accompanied with forms for validating any parsed
            ;; LAMBDA-SIGNATURE onto any single lambda list syntax.
            (update-context (quote &key))
            (add-param param)
            (when (and (eq param kwd-last) kwd-others-p)
              (add-to-buffer (quote &allow-other-keys))))
            ;; FIXME - the other-handler support needs testing, for
            ;; this unparser function
           (t (mapcar #'add-to-buffer
                      (funcall other-handler param signature))))
         (setq param (param-next param)))

      (coerce buf 'list))))



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
                 (list &optional function)
                 (values lambda-signature &optional))
                compute-lambda-signature))

(defun compute-lambda-signature (lambda-list &optional other-handler)
  ;;
  ;; NB: OTHER-HANDLER - if provided, a function accepting three
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
  ;;     COMPUTE-LAMBDA-SIGNATURE
  ;;
  ;; Should return either a PARAM object - such that this function,
  ;; COMPUTE-LAMBDA-SIGNATURE, will then add to the OTHER-SUSBET
  ;; internally -- or return the value NIL, indicating that no value is
  ;; to be stored for the symbol that was provided to the function's
  ;; first argument.
  ;;
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
  ;; see also:  COMPUTE-LAMBDA-LIST
  ;;
  ;; NB: The OTHER-HANDLER function may be for adding support for &AUX,
  ;; &WHOLE, &ENV, and implementation-specific lambda list keywords -
  ;; extesionally, portably, & without any requirement for rewriting the
  ;; function, COMPUTE-LAMBDA-SIGNATURE.

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
        context
        (other-keywords
         (set-difference lambda-list-keywords
                         ;: NB: These are all handled internally here:
                         '(&optional &key &allow-other-keys &rest)
                         :test #'eq)))
    (declare (type cons other-keywords))

    ;; see also: LTP/COMMON:DEFUN*

    (macrolet ((update-for-param (elt)
                 `(progn
                    (unless first
                      (setq first ,elt))
                    (when last
                      (setf (param-next last) ,elt))
                    (setq last ,elt)))
               (mk-buffer (&optional (len 0))
                 (make-array len :fill-pointer len
                             :adjustable t))
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
                    (update-for-param ,elt)
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
               (setq context which))
             (mkerr (msg)
               (lambda-list-syntax-error lambda-list msg))

             (process-symbol-expr (elt)
               (declare (type symbol elt))
               (case elt
                 (&optional
                  (set-state elt)
                  (cond
                    (optional-subtree
                     (mkerror "Lambda list contains more than &OPTIONAL keyword"))
                    (t
                     (setq optional-subtree (mk-buffer 0)))))
                 (&key
                  (set-state elt)
                  (cond
                    (keyword-subtree
                     (mkerror "Lambda list contains more than &KEY keyword"))
                    (t
                     (setq keyword-subtree (mk-buffer 0)))))
                 (&allow-other-keys
                  (unless keyword-subtree
                    (mkerr "Lambda list contains &ALLOW-OTHER-KEYS ~
other than after &KEY symbol"))
                  (when allow-other-keys
                    (mkerr "Lambda list contains more than one ~
&ALLOW-OTHER-KEYS symbol"))
                  (setq allow-other-keys t)
                  (set-state elt))
                 (&rest
                  ;; NB: &BODY not handled here.
                  ;;
                  ;; Handling for &BODY may be defined as to reuse the
                  ;; exixting REST-PARAM class
                  (when rest-p
                    (mkerr "Lambda list contains more than one &REST symbol"))
                  (setq rest-p t)
                  (set-state elt))
                 ((position elt other-keywords :test #'eq)
                  (process-other-expr elt))
                 (t
                  ;; proceed under the current parser state
                  (case context
                    ((nil)
                     (add-to-required-buffer (make-required-param elt)))
                    (&optional
                     (add-to-optional-buffer (make-optional-param elt)))
                    (&key
                     ;; FIXME check for &optional / warn on ambiguity
                     (add-to-key-buffer
                      (make-keyword-param
                       elt
                       :keyword-name (intern (symbol-name elt)
                                             :keyword))))
                    (&rest
                     (when rest-param
                       (mkerr "Lambda list contains more than one ~
&REST parameter"))
                     (setq rest-param
                           (make-rest-param elt context))
                     (update-for-param rest-param))

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
                      (add-to-key-buffer (make-keyword-param
                                          param-name
                                          :keyword-name key-name
                                          :initform initform
                                          :initfunction initfunction
                                          :value-p-name value-p
                                          :value-p-p v-p-p)))))
                 (t (process-other-expr elt))))

             (process-other-expr (elt)
               (cond
                 (other-handler
                  (funcall (the function other-handler)
                           elt context lambda-list))
                 (t
                  ;; FIXME: Improve this error's  message text
                  (mkerr "Syntax not supported")))))

      (do-cons (elt rest lambda-list)
        (unless (listp rest)
          (mkerr "Syntax not supported - not a well formed list"))
        (etypecase elt
          (symbol (process-symbol-expr elt))
          (cons (process-cons-expr elt))
          (t (process-other-expr elt))))

      (%mk-lambda-signature
       :lambda-list lambda-list
       :first first
       :required (when required-subtree
                   (%mk-required-param-subset
                    (coerce required-subtree 'simple-vector)))
       :optional (when optional-subtree
                   (%mk-optional-param-subset
                    (coerce optional-subtree 'simple-vector)))
       :keyword (when keyword-subtree
                  (%mk-keyword-param-subset
                   (coerce keyword-subtree 'simple-vector)
                   allow-other-keys))
       :other (when other-subtree
                (%mk-other-param-subset
                 (coerce other-subtree 'simple-vector)
                 allow-other-keys))
       :rest rest-param))

    )))

(eval-when ()
  ;; test for a limited subset of lambda list syntax

  (defparameter *l*
    (compute-lambda-signature
     '(a b &optional q &rest other &key frob &allow-other-keys)))

  (values
   (lambda-signature-first-param *l*)
   ;; (lambda-signature-required-param-subset *l*)
   ;; (lambda-signature-optional-param-subset *l*)
   (lambda-signature-rest-param *l*)
   ;; (lambda-signature-keyword-param-subset *l*))
   )

  (compute-lambda-list
   (compute-lambda-signature
    '(a b &optional q &rest other &key frob &allow-other-keys)))

  (compute-lambda-list
   (compute-lambda-signature
    '(a b &optional q &rest other &key frob (fixme tbd))))

  ;; FIXME - empty key set not being unparsed correctly
  (describe (compute-lambda-signature
               '(a b &optional q &rest other &key)))

  (compute-lambda-list
   (compute-lambda-signature
    '(a b &optional q &rest other &key))
   )
)



;; -- Partial MOP Interop

(defclass generic-op (standard-generic-class)
  ((lambda-signature
    :reader generic-op-lambda-signature
    :type lambda-signature)))

(defclass generic-op-method (standard-method)
  ())

(defmethod generic-function-method-class ((genop generic-op))
  (find-class 'generic-op-method))

;; ---



(defgeneric compute-ftype-params-type (genop)
  #+FIXME
  (:method ((genop generic-op))
    (let ((signature (generic-function-lambda-signature genop)))
      ;; see also: defun*
      ;;
      ;; Note, the reusable COMPUTE-LAMBDA-SIGNATURE function defined above
      ;;
      ;; - would not be needed here, directly, for a GENERIC-OP
      ;;   in which (GENERIC-OP-SIGNATURE GENOP)
      ;;   is assumed to return a LAMBDA-SIGNATURE
      ;;
      ;; - may be of some use if implementing a method for this
      ;;   function, specialized directly onto STANDARD-GENERIC-CLASSa
      ;;
      ;; - NB handle the LAMBDA-SIGNATURE-REQUIRED-PARAM-SUBSET mainly,
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
