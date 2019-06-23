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


;; --

(defstruct (param
             (:constructor))
  (name (quote #.(make-symbol "unbound"))
        :type symbol
        :read-only t))


(defstruct (param-subset
             (:constructor))
  (members #.(make-array 0)
           :type simple-vector
           :read-only t))

(defstruct (required-param
             (:include param)
             (:constructor make-required-param (name))))

(defstruct (required-param-subset
             (:include param-subset)
             (:constructor %make-required-param-subset (members))))

(defstruct (optional-param
             (:include param)
             (:constructor make-optional-param (name))))

(defstruct (optional-param-subset
             (:include param-subset)
             (:constructor %make-optional-param-subset (members))))

(defstruct (rest-param
             (:include param)
             (:constructor make-rest-param (name)))
  #+THISv2 ;; FIXME add parser/unparser support for this
  (kind (quote &rest)
        :type symbol ;; (member &rest &body) ;; typically
        :read-only t)
  )

(defstruct (keyword-param
             (:include param)
             (:constructor make-keyword-param (name)))
  ;; FIXME - need more structure here
  )


(defstruct (keyword-param-subset
             (:include param-subset)
             #+THISV2
             (:constructor %make-keyword-param-subset
                           (members &optional allow-other-keys))
             )
  ;; NB: FIXME: Update this model to indicate &ALLOW=OTHER-KEYS here
  #+THISV2
  (allow-other-keys
   nil
   :type boolean
   :read-only t))


#+NIL ;; unused as yet - see annotations, subsq.
(defstruct (aux-param
             (:include param)
             (:constructor make-aux-param (name))))


(defstruct (lambda-signature
             #-THISv2
             (:constructor %make-lambda-signature
                           (params &optional allow-other-keys))
             #+THISv2
             (:constructor %make-lambda-signature
                           (&optional required-param-subset
                                      optional-param-subset
                                      keyword-param-subset
                                      rest-param))
             )
  #-THISv2
  (params
   #.(make-array 0)
   :type simple-vector
   :read-only t)
  #-THISv2
  (allow-other-keys
   nil
   :type boolean
   :read-only t)
  #+THISv2
  (required-param-subset
   nil
   :type (or required-param-subset null)
   :read-only t)
  #+THISv2
  (optional-param-subset
   nil
   :type (or optional-param-subset null)
   :read-only t)
  #+THISv2
  (keyword-param-subset
   nil
   :type (or keyword-param-subset null)
   :read-only t)
  #+THISv2
  (rest-param
   nil
   :type (or rest-param null)
   :read-onky t)
  ;; TBD in extensions:
  ;; - AUX-PARAM-SUBSET - specifically for anonymous lambdas & defun forms
  ;; - ENV-PARAM - specifically for macro lambda forms
  ;; - WHOLE-PARAM - cf. SETF forms and similar
  ;; ... and interop. for implementation-specific lambda forms
  ;; ... with further specialization, in applications.
  ;;     NB: Optional parameter defaulting for DEFTYPE lambda signatures
  ;;     NB: Parameter name matching for DEFSTRUCT lambda signatures and
  ;;         structure-class slot definitions
  ;;     ... etc.
  ;;
  ;; NB/Topic: User Interfaces & Accessibility for Lambda Model Definitions
  ;;
  ;; NB: This was originally defined for supporting implementation of
  ;; GENERIC-OP, begeinning with a method onto the standard generic
  ;; function, COMPUTE-FTYPE-PARAMS-TYPE. As such, the syntax supported
  ;; with this protocol is -- at this time -- limited to the lambda list
  ;; syntax supported for standard generic functions.
  )


(declaim (ftype (function (sequence &optional t)
                          (values lambda-signature &optional))
                make-lambda-signature))

(defun make-lambda-signature (params &optional allow-other-keys)
  (%make-lambda-signature (coerce params 'simple-vector)
                          (and allow-other-keys t)))


;; ----


(declaim (ftype (function (lambda-signature) (values list &optional))
                compute-lambda-list))

(defun compute-lambda-list (signature)
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



(declaim (ftype (function (list) (values lambda-signature &optional))
                compute-lambda-signature))

(defun compute-lambda-signature (lambda-list)
  (let ((buf (make-array (length lambda-list) ;; NB: approx. final length
                         :fill-pointer 0 :adjustable t))
        #+THISv2 required-subtree
        #+THISv2 keyword-subtree
        #+THISv2 optional-subtree
        allow-other-keys
        rest-p
        rest-param
        context)
    ;; see also: LTP/COMMON:DEFUN*

    (labels ((add-to-buffer (elt #-THISv2 &optional #-THISv2 (which buf)
                                 #+THISv2 which)
               (vector-push-extend elt which))
             (set-state (which)
               (setq context which))
             (parse-signature (llist)
               ;; FIXME - use DO-CONS and err for any non-list CDR
               (dolist (elt llist buf)
                 (etypecase elt
                   (symbol
                    (case elt
                      (&key
                       #+THISv2
                       (setq keyword-subtree
                             (make-array 0 :fill-pointer 0 :adjustable t))
                       (set-state elt))
                      (&optional
                       #+THISv2
                       (setq optional-subtree
                             (make-array 0 :fill-pointer 0 :adjustable t))
                       (set-state elt))
                      (&allow-other-keys
                       #+THISv2
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
                       #+THISv2
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
                          #+THISv2
                          (unless required-subtree
                            (setq required-subtree
                                  (make-array 1 :fill-pointer 0
                                              :adjustable t)))
                          (add-to-buffer (make-required-param elt)
                                         #+THISv2
                                         required-subtree))
                         (&optional
                          (add-to-buffer (make-optional-param elt)
                                         #+THISv2 optional-subtree))
                         (&key
                          ;; FIXME - need further processing here,
                          ;; for specialized &KEY syntax forms

                          ;; FIXME check for &optional / warn on ambiguity
                          (add-to-buffer (make-keyword-param elt)
                                         #+THISv2 keyword-subtree))
                         (&rest
                          (when rest-param
                            (lambda-list-syntax-error
                             lambda-list "Lambda list contains ~
more than one &REST parameter"))
                          #-THISv2
                          (add-to-buffer (make-rest-param elt))
                          #-THISv2
                          (setq rest-param t)
                          #+THISv2
                          (setq rest-param (make-rest-param elt))
                          )
                         ;; FIXME: Specialize the following error,
                         ;; and improve the message text - same for the
                         ;; second, below.
                         (t (lambda-list-syntax-error lambda-list
                                                      "Syntax not supported"))))
                      )) ;; SYMBOLP ELT
                   (cons
                    ;; FIXME (??)
                    (lambda-list-syntax-error lambda-list
                                              "Syntax not supported"))
                   (t
                    (lambda-list-syntax-error lambda-list
                                              "Unrecognized syntax"))
                   ))))
      ;; FIXME - note other FIXME notes here
      #-THISv2
      (make-lambda-signature (parse-signature lambda-list)
                             allow-other-keys)
      #+THISv2
      (%make-lambda-signature (when required-subtree
                                (%make-required-param-subset
                                 (coerce required-subtree 'simple-vector)))
                              (when optional-subtree
                                (%make-optional-param-subset
                                 (coerce optional-subtree 'simple-vector)))
                              (when keyword-subtree
                                (%make-keyword-param-subset
                                 (coerce optional-subtree 'simple-vector)
                                 allow-other-keys))
                              rest-param))
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
