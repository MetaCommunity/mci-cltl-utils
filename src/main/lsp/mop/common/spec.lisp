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

#+NIL ;; unused macro, in this revision
(defmacro do-cons ((first rest whence &optional return) &body body)
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
                   (while (progn #+DEBUG (warn "ITERATE ~D ~D ~D"
                                               specializer-offset depth (length known))
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

(eval-when (:compile-toplevel :execute)
  (defmacro lform (val)
    `(load-time-value ,val :constant t))
  )

(defstruct (param
             (:constructor))
  (name
   (lform (make-symbol "Unbound Name"))
   :type symbol
   :read-only t)
  #+NEXT_CHANGE
  (next
   nil ;; indicating, in effect, "End of lambda list"
   :type (or param null)
   ;; NB: Not defined as read-only, due to how PARAM subclases are
   ;; initialized within COMPUTE-LAMBDA-SIGNATURE
   ;;
   ;; If any application modifies the value of this slot once it's
   ;; bound, the behaviors will be unspecified for any procedure
   ;; unparsing a lambda list from any containing LAMBDA-SIGNATURE.
   :read-only nil)
  )


(defstruct (initialized-param
             (:include param)
             (:constructor))
  ;; NB: Usage by extension vis a vis CL:&KEY, CL:&OPT params
  (initform
   (lform (make-symbol "Unbound Initform"))
   :type t
   :read-only t)
  (initfunction
   (lform #'(lambda () (error "Unbound Initfunction")))
   :type function
   :read-only t)
  (value-p-name
   (lform (make-symbol "Unbound Suppllied-P-Name"))
   :type symbol
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
                            value-p-name))))

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
                            (keyword-name
                             (intern (symbol-name name) :keyword)))))
  (keyword-name
   (lform "Unbound Keyword Name")
   :type symbol
   :read-only t))

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



(defstruct (lambda-signature
             (:constructor %mk-lambda-signature
                           (&key
                            ((:required required-param-subset) nil)
                            ((:optional optional-param-subset) nil)
                            ((:keyword keyword-param-subset) nil)
                            ((:rest rest-param) nil)
                            )))
  ;; Ed. NB: See remarks about the design of this lambda list parser
  ;; model, in the COMPUTE-LAMBDA-SIGNATURE source form

  (first-param
   nil
   :type param
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
   :type (or other-param-subset-set null)
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

(defun compute-lambda-list (signature ;; <- as a LAMBDA-SIGNATURE
                            #+TBD &optional #+TBD other-handler)
  ;; NB: OTHER-HANDLER - if provided, a function accepting one argument,
  ;; i.e a PARM object "not otherwise handled" below. Should return a
  ;; list of symbols (optionally null) such that ... TBD ordering for
  ;; OTHER-SUBSET onto the original lambda list expression.
  ;;
  ;; see also:  COMPUTE-LAMBDA-SIGNATURE
  (let* ((params (lambda-signature-params signature))
         (buf (make-array (length params) ;; NB: approx. final length
                         :fill-pointer 0 :adjustable t))
        restp
        context)
    (labels ((add-to-buffer (elt)
               (vector-push-extend elt buf))
             (update-context (which)
               (unless (eq context which)
                 (setq context which)
                 (add-to-buffer which))))
      (do-vector (param params)
        (let ((name (param-name param)))
        (etypecase param
          ;; NB: for now, not parsing for other types of lambda kwd
          (required-param
           (add-to-buffer name))
          ;; FIXME add parseer subtree for OPTIONAL-PARAM-SUBSET
          (optional-param
           (update-context (quote &optional))
           (add-to-buffer name))
          (rest-param
           (when restp
             (error "More than on &REST param in ~S" signature))
           (update-context (quote  &rest))
           (setq restp t)
           (add-to-buffer name))
          ;; FIXME add parseer subtree for KEY-PARAM-SUBSET
          (keyword-param
           ;; FIXME - need further processing here
           (update-context (quote &key))
           (add-to-buffer name))
          )))

      ;; FIXME - cheap hack, as yet
      (when (lambda-signature-allow-other-keys signature)
        (add-to-buffer (quote &allow-other-keys)))

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
                 (list #+TBD &optional #+TBD function)
                 (values lambda-signature &optional))
                compute-lambda-signature))

(defun compute-lambda-signature (lambda-list
                                 #+TBD &optional #+TBD other-handler)
  ;;
  ;; NB: OTHER-HANDLER - if provided, a function accepting TBD arguments,
  ;; at the least:
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
  ;; - the previous PARAM object parsed from the LAMBDA-LIST expression
  ;;   (??) NB: Not actually needed, as the PARAM objects are not doubly
  ;;   linked, in this design.
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

  ;; Ed. NB: Evaluation environments and initializer-param initfunctions

  (let (required-subtree
        keyword-subtree
        optional-subtree
        #+TBD other-subtree
        allow-other-keys
        rest-p
        rest-param
        #+NEXT_CHANGE last
        context)
    ;; see also: LTP/COMMON:DEFUN*

    (macrolet ((add-to-buffer (elt which)
                 (with-symbols (%which)
                 `(let ((,%which ,which))
                    #+NEXT_CHANGE (setf (param-next last) ,%which)
                    (cond
                      (,%which
                       (vector-push-extend ,elt ,%which))
                      (t (setq ,which
                               (make-array 1 :fill-pointer 1
                                           :adjustable t))
                         (setf (aref ,which 0)
                               ,elt)))))))

    (labels ((add-to-required-buffer (elt)
               (add-to-buffer elt required-subtree))
             (add-to-optional-buffer (elt)
               (add-to-buffer elt optional-subtree))
             (add-to-key-buffer (elt)
               (add-to-buffer elt keyword-subtree))
             #+TBD
             (add-to-other-buffer (elt)
               (add-to-buffer elt other-subtree))

             (set-state (which)
               (setq context which))
             (parse-signature (llist)
               ;; FIXME - use DO-CONS and err for any non-list CDR
               (dolist (elt llist buf)
                 (etypecase elt
                   (symbol
                    (case elt
                      (&key
                       (setq keyword-subtree
                             (make-array 0 :fill-pointer 0 :adjustable t))
                       (set-state elt))
                      (&optional
                       (setq optional-subtree
                             (make-array 0 :fill-pointer 0 :adjustable t))
                       (set-state elt))
                      (&allow-other-keys
                       (unless keyword-subtree
                         (lambda-list-syntax-error lambda-list
                                                   "Lambda list contains ~
&ALLOW-OTHER-KEYS other than after &KEY symbol"))

                       (when allow-other-keys
                         (lambda-list-syntax-error lambda-list
                                                   "Lambda list contains ~
more than one &ALLOW-OTHER-KEYS symbol"))

                       (setq allow-other-keys t)
                       (set-state elt))

                      ((&rest)
                       (when rest-p
                         (lambda-list-syntax-error lambda-list
                                                  "Lambda list contains ~
more than one &REST symbol"))
                       (setq rest-p t)
                       (set-state elt))

                      ;; proceed under current parser state
                      (t
                       (case context
                         ((nil)
                          (unless required-subtree
                            (setq required-subtree
                                  (make-array 1 :fill-pointer 0
                                              :adjustable t)))
                          (add-to-required-buffer (make-required-param elt))))

                         (&optional
                          (add-to-optional-buffer (make-optional-param elt)))

                         (&key
                          ;; FIXME - need further processing here,
                          ;; for specialized &KEY syntax forms

                          ;; FIXME check for &optional / warn on ambiguity
                          (add-to-key-buffer (make-keyword-param elt)))

                         (&rest
                          (when rest-param
                            (lambda-list-syntax-error
                             lambda-list "Lambda list contains ~
more than one &REST parameter"))
                          (setq rest-param (make-rest-param elt context))
                          )
                         ;; FIXME: Specialize the following error,
                         ;; and improve the message text - same for the
                         ;; second, below.
                         (t (lambda-list-syntax-error lambda-list
                                                      "Syntax not supported"))))
                      )) ;; SYMBOLP ELT
                   (cons
                    ;; FIXME - parse per &OPTIONAL and &KEY context
                    ;;
                    ;; FIXME - use OTHER-SUBSET when OTHER-HANDLER is
                    ;; provided, else err
                    (lambda-list-syntax-error lambda-list
                                              "Syntax not supported"))
                   (t
                    ;; FIXME - use OTHER-SUBSET when OTHER-HANDLER is
                    ;; provided, else err
                    (lambda-list-syntax-error lambda-list
                                              "Unrecognized syntax"))
                   ))))
      (%mk-lambda-signature
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
       #+TBD :other #+TBD (when other-subtree
                            (%mk-other-param-subset
                             (coerce other-subtree 'simple-vector)
                             allow-other-keys))
       :rest rest-param))
    ))

;; (compute-lambda-signature '(a b &optional q &rest other &key frob &allow-other-keys))

;; (compute-lambda-list (compute-lambda-signature '(a b &optional q &rest other &key frob &allow-other-keys)))

;; (compute-lambda-list (compute-lambda-signature '(a b &optional q &rest other &key frob (fixme tbd))))

;; (compute-lambda-list (compute-lambda-signature '(a b &optional q &rest other &key)))


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
