
(in-package #:ltp/common)

(defpackage #:ltp/common/mop/enum
  (:use #:ltp/common/singleton
        #:ltp/common/mop
        #:ltp/common
        #:cl))

(in-package #:ltp/common/mop/enum)
;; ^ tmp location for this src

;; -- ENUM extension onto LTP SINGLETON

;; FIXME: Move this to a separate source file;
;; consider defining this in LTP itself

(defgeneric %storage-context-members (enum))
(defgeneric (setf %storage-context-members) (new enum))

(defclass enum ()
  ;; FIXME: Provide a guard (mutex e.g) onto the %MEMBERS buffer
  ;;
  ;; Use R/W mutexes, when such an interface is provided by the
  ;; implementation - e.g in CCL
  ((%members
    ;; access varies depending on the subclass, for each of DYNAMIC-ENUM
    ;; and STATIC-ENUM classes
    ;;
    ;; should not be unbound in implementation classes
    :type sequence)
   (member-class
    :initarg :member-class
    ;; :access :read-only
    ;; should not be unbound
    :type class-designator)
   (member-key-slot
    :initarg :member-key-slot
    ;; :access :read-only
    ;; should not be unbound
    :type symbol)
   (member-key-accessor
    :initarg :member-key-accessor
    ;; :access :read-only
    ;; should not be unbound
    :type function)
   (member-find-function
    :initarg :member-find-function
    ;; :access :read-only
    ;; should not be unbound
    :type function)
   ))


(defmethod shared-initialize :around ((instance enum) slots
                                      &rest initargs
                                      &key
                                        (members nil memp)
                                        (%members nil %memp)
                                        &allow-other-keys)
  (declare (ignore instance slots initargs))

  ;; In effect, validate INITARGS before dispatching to primary method
  (when (and memp %memp)
    (simple-program-error
     "~<Conflicting initialization arguments:~>~< ~s (value ~s)~>~
~< and ~s (value ~s)~>"
     :members members :%members members))

  (call-next-method))

;; --


(defclass dynamic-enum (enum)
  ;; Assumption: Static ENUM definitions may be initialized once, then
  ;; would be read-only
  ;;
  ;; FIXME: Ensure such a semantics, for the ENUM %MEMBER slot of a
  ;; STATIC-ENUM class
  ((member-register-function
    ;; :access :read-only
    ;; may be unbound
    :initarg :member-register-function
    :type function)
    (member-remove-function
    ;; :access :read-only
    ;; may be unbound
    :initarg :member-remove-function
    :type function)))


(defclass array-storage-context ()
  ((%members
    :type array)))


(defclass expandable-vector-storage-context (array-storage-context)
  ((%members
    :initarg :%members
    :accessor %storage-context-members ;; see also: :SYNCHRONIZE (NB)
    ;; :synchronize (:read :write)
    :type (array t (*)))))


(defmethod initialize-storage ((context expandable-vector-storage-context))
  (unless (slot-boundp context '%members)
    (setf (%storage-context-members context)
          (make-array 8 :fill-pointer 0)))
  (values context))


;; -- Dynamic Deallocation for Storage Objects

(defgeneric storage-empty-object (storage #+NIL context))
;; cf. DEALLOCATE-STORAGE

(defgeneric deallocate-storage (obj #+NIL context)
  ;; NB: This assumes that every reachable object within OBJ can be
  ;; destructively modified. As such, this does not operate on
  ;; implementation classes.
  ;;
  ;; FIXME This does not operate onto any manner of portable reference
  ;; pointers, in Common Lisp
  (:method ((buff null))
    ;; no-op
    (values))
  (:method ((buff cons))
    ;; NB: does not perform any circular list detection.
    ;;
    ;; Any calling program should ensure that there are no circular
    ;; lists in the respective OBJ
    (deallocate-storage (cdr buff))
    (setf (cdr buff) nil)
    (deallocate-storage (car buff))
    (setf (car buff) nil)
    (values t))
  (:method  ((buff simple-string))
    ;; no-op
    (values))
  (:method  ((buff simple-bit-vector))
    ;; no-op
    (values))

  #++NIL
  (:method ((buff vector))
    (multiple-value-prog1 (call-next-method)
      (when (array-has-fill-pointer-p buff)
        (setf (fill-pointer buff) 0))))
  #+NIL
  (:method ((buff array))
    (dotimes (n (apply #'* (array-dimensions buff)) (values t))
      (deallocate-storage (row-major-aref buff n))
      (setf (row-major-aref buff n)
            (storage-empty-object buff #+NIL context))))

  (:method ((buff fixnum))
    ;; no-op
    (values))
  (:method ((buff complex))
    ;; portably, no-op [FIXME]
    (values))
  (:method ((buff symbol) #+NIL (context t))
    ;; NO-OP in this context [PROTOTYPE]
    (values))
  #+NIL
  (:method ((buff symbol))
    ;;; FIXME - limit this onto a specific context
    ;;
    ;; NB: This will not affect any existing references to a symbol
    (unintern buff)
    (values t)))

;; (deallocate-storage (make-array 10 :fill-pointer 10) nil)


;; --

(defmethod close-storage ((context expandable-vector-storage-context))
  (cond
    ((slot-boundp context '%members)
     (deallocate-storage (%storage-context-members context) #+NIL context)
     (slot-makunbound context '%members)
     (values t))
    (t (values))))


(defclass simple-dynamic-enum (expandable-vector-storage-context dynamic-enum)
  ())


(defmethod shared-initialize ((instance simple-dynamic-enum) slots
                              &rest initargs
                              &key (members nil memp) (%members nil %memp)
                                &allow-other-keys)
  (let (initargs-updated)
    (macrolet ((next-method ()
                 `(cond
                    (initargs-updated (apply #'call-next-method
                                             instance slots initargs))
                    (t (call-next-method))))
               (update (arg value)
                 `(prog1 (setf (getf initargs ,arg) ,value)
                    (setq initargs-updated t))))
      (when memp
        (when %memp
          (simple-program-error
           "~<Conflicting initialization arguments:~>~< ~s (value ~s)~>~
~< and ~s (value ~s)~>"
           :members members :%members members))

        ;; ensure that an appropriate adjustable vector is created for the
        ;; provided :members value
        (let ((len (length members)))
          (update :%members
                  (make-array len
                              :element-type t
                              :initial-contents members
                              :fill-pointer len))))
      (next-method))))



(defgeneric enum-members (enum)
  (:method ((enum simple-dynamic-enum))
    (when (slot-boundp enum '%members)
      (coerce (the (array t (*)) (%storage-context-members enum))
              'list))))


(defgeneric (setf enum-members) (new-value enum)
  (:method ((new-value list) (enum simple-dynamic-enum))
    (setf (enum-members enum)
          (coerce (the list new-value) 'simple-vector)))
  (:method ((new-value vector) (enum simple-dynamic-enum))
    (cond
      ((array-has-fill-pointer-p new-value)
       (setf (%storage-context-members enum) new-value))
      (t
       (let ((len (length new-value)))
         (setf (%storage-context-members enum)
               (make-array len
                           :element-type t
                           :fill-pointer len
                           :initial-contents new-value)))))))


(declaim (ftype (function (t) (values list &optional))
                enum-members))
(declaim (ftype (function (sequence t) (values sequence &optional))
                (setf enum-members)))
;; ^ FIXME limit FTYPE arg-type decls onto defined methods, once declared

(eval-when () ;; TD: Instance tests - DYNAMIC-ENUM Initialization, Accessors
  (defparameter *enum*
    (make-instance 'dynamic-enum :members (list "A" "B" "C" "D")))

  (enum-members *enum*)

  (%storage-context-members *enum*)

  (vector-push-extend "E" (%storage-context-members *enum*))

  (enum-members *enum*)

  (setf (enum-members *enum*)
        (reverse (enum-members *enum*)))

  (enum-members *enum*)

  (let* ((buf (%storage-context-members *enum*))
         (n (length buf))
         (newbuf
          (make-array n
                      :element-type t
                      :fill-pointer n
                      :initial-contents (nreverse buf))))
    (values
     (setf (enum-members *enum*) newbuf)
     (eq (%storage-context-members *enum*) newbuf)))
  ;; last value => T


)



;; ----

#+NIL ;; simple example NB
(defsingleton dynamic-enum-class (dynamic-enum)
  ())


;; --

(defclass static-enum (enum)
  ())


(defclass simple-vector-storage-context (array-storage-context)
  ;; NB: Leaving the semantics undefined for INITIALIZE-STORAGE
  ;;     of a SIMPLE-VECTOR-STORAGE-CONTEXT
  ;;
  ;;     Similarly, for CLOSE-STORAGE
  ((%members
    :accessor %storage-context-members
    ;; :read-only t
    :type simple-vector)))


(defclass simple-static-enum (simple-vector-storage-context static-enum)
  ())


(defmethod shared-initialize ((instance simple-static-enum) slots
                              &rest initargs
                              &key (members nil memp) #+NIL (%members nil %memp)
                                &allow-other-keys)
  (let (initargs-updated)
    (macrolet ((next-method ()
                 `(cond
                    (initargs-updated (apply #'call-next-method
                                             instance slots initargs))
                    (t (call-next-method))))
               (update (arg value)
                 `(prog1 (setf (getf initargs ,arg) ,value)
                    (setq initargs-updated t))))
      (when memp
        ;; ensure that an appropriate simple vector is created for the
        ;; provided :members value
        (update :%members (coerce members 'simple-vector)))

      (next-method))))

(defmethod enum-members ((enum simple-static-enum))
  (when (slot-boundp enum '%members)
    (coerce (the simple-vector (%storage-context-members enum))
            'list)))

;; TD: Instance tests - STATIC-ENUM Initialization, Accessors



;; ----

#+NIL ;; simple example NB
(defsingleton static-enum-class (static-enum)
  ())

;; NB
;; (class-finalized-p (find-class 'static-enum-class))
;; => T

;; --

(eval-when ()
(defmacro define-singleton-enum (name (&rest supertypes)
                                         (&rest slots)
                                            (&rest decls)
                                 &rest params)
  ;; API Constructor Macro

  ;; Notes
  ;;
  ;; - Params: Syntax
  ;;
  ;;   - Refer to direct slot definitions of the classes
  ;;     ENUM and DYNAMIC-ENUM. Excepting the slot definition for the
  ;;     %MEMBERS slot, those class direct slots' other initialization
  ;;     aruments may be provided via the PARAMS section of this macro.
  ;;
  ;;  - Furthermore - NB: DEFINE-SINGLETON-ENUM somewhat like DEFSTRUCT
  ;;     (:CONC-SUFFIX &optional <NAME>)
  ;;     (:RESISTER-CONC-NAME &optional <NAME>)
  ;;     (:FIND-CONC-NAME &optional <NAME>)
  ;;     (:REMOVE-CONC-NAME &optional <NAME>)
  ;;
  ;;     NB: It would be an error to specify a (:CONC-SUFFIX) with null
  ;;     <NAME> and -- in the same DEFINE-SINGLETON-ENUM -- null <NAME>
  ;;    for any one or more of the :RESISTER-CONC-NAME, :FIND-CONC-NAME
  ;;    and :REMOVE-CONC-NAME options in PARAMS

  ;; NB: This macro will destructively modify a copy of PARAMS
  (let ((%params (copy-list params))
        conc-suffix
        reg-prefix
        reg-name
        find-prefix
        find-name
        remove-prefix
        remove-name
        ;; subclasses
        )
    (macrolet ((pop-param (name)
                  `(let ((n (position (quote ,name) %params
                                      :key #'car
                                      :test #'eq)))
                     (cond
                       (n (let ((p (nth n params)))
                            (setq params
                              (delete p %params :test #'eq))
                            (values p)))
                       (t (values nil))))))
      ;; replace nested MV GET-ONE-PARAM calls with POP-PARAM

      (labels ((ensure-symbol-name (name)
                 (etypecase name
                   (string name)
                   (symbol (symbol-name name)))))
        (setq conc-suffix (pop-param conc-suffix))

        (let* ((conc-rest (cdr conc-suffix))
               (%conc-suffix
                (cond
                  ((and conc-suffix conc-rest)
                   (destructuring-bind (suffix &rest n-a) conc-rest
                     (when n-a
                       (simple-program-error
                        "~<Extraneous information ~
in DEFINE-SINGLETON-ENUM~>~< (:CONC-SUFFIX . ~S)~>" conc-rest))

                     (when suffix
                       (ensure-symbol-name suffix))))
                  (conc-suffix
                   ;; i.e (:CONC-SUFFIX)
                   ;; => do not define any default enum accessors
                   ;; NB: Ssame effect as (:CONC-SUFFIX NIL)
                   (values nil))
                  (t (setq conc-suffix (concatenate 'simple-string
                                                    "-" (symbol-name name)))))))
          (labels ((mk-default-name (prefix)
                     (cond
                       (%conc-suffix
                        (intern (concatenate 'simple-string
                                             (symbol-name prefix)
                                             %conc-suffix)))
                       (t (intern (symbol-name prefix))))))

            (setq reg-form (pop-suffix register-function))

            (unless reg-form
              (setq reg-form (mk-default-name '#:register)))

            (cond
              ((and regp (listp reg-form) (listp (cadr reg-form))))
              ;; ^ no-op - interpret value as an implicit defun form
              ((and regp (null reg-form)))
              ;; ^ no-op - do not define or store a :register-function
              (reg-form
               ;; ^ function name

               ;; create implicit defun (??) [TBD] (NB At least, store ref to
               ;; fn - may be forward-referenced) (??)

               #+TD ;; TBD: DEFUN in the macroexpansion ?
               (let ((reg-lambda (compute-default-registration-lambda ...)))
                 (setq reg-form `(,name ,@(cdr reg-lamba))))))

            (setq find-form
                  (pop-param :find-function))
            (unless find-form
              (setq find-form (mk-default-name '#:find)))


            (setq remove-form
                  (pop-param :remove-function))

            (unless remove-form
              ;; FIXME: This REMOVE name with null CONC-SUFFIX
              ;; -- i.e in params (:CONC-SUFFIX) -- would typically
              ;; override CL:REMOVE. This may typically represent
              ;; an unwanted side effect of the evaluation.
              ;;
              ;; Using a prefix #:DELETE would not improve the
              ;; matter,
              ;;
              ;; As unlikely as (:CONC-SUFFIX) in PARAMS may be, it
              ;; should be more carefully addressed - in the
              ;; interest of portable systems programming.
              ;;
              ;; Consider adding to the PARAMS syntax:
              ;; (:RESISTER-CONC-NAME &optional <NAME>)
              ;; (:FIND-CONC-NAME &optional <NAME>)
              ;; (:REMOVE-CONC-NAME &optional <NAME>)
              (setq remove-form (mk-default-name '#:remove)))

            ;; (compute-default-remove-lambda ...)


            ;; TBD: for each SPEC in DECLS: Process SPEC via method
            ;; dispatch (??) Note, hoewver, that this would serve to
            ;; require that at least one class would be defined for that
            ;; specialization, before this macro is evaluated in the
            ;; compiler environment. This, at least, would serve to allow
            ;; for an extensible protocol for handlign the CDR of each
            ;; SPEC in a syntax specific to the individual enumerated type

            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (defabstract ,name (,@supertypes)
                 (,@slots)
                 ,@params)

               ;; ... defsingleton for each SPEC in DECLS ...

               ;; ... decls (FTYPE) and defuns for each enum access
               ;; function (register, find, remove)

               ;; return the defined class
               ))))))))


;; #+NIL
;; (eval-when () ;;  enum-proto.lisp - Basic prototyping/Proof of concept

  ;; -- Implementation Notes
  ;;
  ;; NB: As something of a quirk for a purpose of simplifying the
  ;; implementation, the following prototype uses a SINGLETON semantics
  ;; for enum container storage.
  ;;
  ;; That may be as to denote: The enum storage object -- typically
  ;; denoted, in the prototype below, as 'whence' -- the storaage object
  ;; is represented by a singleton class. Although the singleton class
  ;; protocol is, at this time, still in a state of revision -- namely,
  ;; in that the exact MAKE-INSTANCE semantics have yet to be completely
  ;; defined for SINGLETON classes -- this, at persent, will serve to
  ;; allow for only one storage per class.
  ;;
  ;; In further development of this protocol -- towards a manner of an
  ;; extensible, template-oriented, prototype -- the WHENCE object, as
  ;; well as any number of accessor functions and types specified below,
  ;; should be -- in effect -- parameterized, within the corresponding
  ;; template.
  ;;
  ;;
  ;; In a manner principally independent of the protocol extension onto
  ;; SINGLETON, this protocol may serve to permit for method definitions
  ;; and dispatching onto the enum storage class itself, e.g TRIVIAL-ENUM.
  ;;
  ;; Regardless, this local object storage and access protocol is
  ;; implemented with principally non-polymorphic functions.
  ;;
  ;; -- Design Notes
  ;;
  ;; In a subsequent revision, a semantics may be defined for specifying
  ;; how any subtypes of an ENUM implementation are to regard any
  ;; objects as may be, in effect, iherited onto any one or more ENUM
  ;; supertypes and corresponding implementation objects. Towards an
  ;; application of such an extension, one might note - in particular -
  ;; the peculiarities of object import semantics in MOF and UML, such
  ;; as may be summarized approximately: Prototype on import.
  ;;
  ;;
  ;; The SINGLETON extension onto ENUM has been defined in a manner such
  ;; as to ensure that the ENUM type and ENUM storage objects -- for any
  ;; single ENUM type -- are represented, in each, by the same object.
  ;;
  ;; The protocol, as illustrated below, utilizes an intermediate ENUM
  ;; member type, such that an ENUM type is -- in effect -- closed onto.

  (deftype name ()
    'symbol)

  (defclass tenum-member ()
    ;; Trivial Enum Member - or TEM

    ;; NB IMPL - DEFSINGLETON  & ENUM KEY INITARG
    ((name
      :initarg :name
      :type name
      :reader tenum-member-name)))


  (defun* make-tem (name)
    (declare (type symbol name)
             (values tenum-member &optional))
    (values (make-instance 'tenum-member :name name)))


  (deftype if-exists-symbol ()
    '(member :override :error :ignore nil))

  (deftype not-found-symbol ()
    '(member :error :ignore nil))


  ;; FIXME - With the present design of DEFSINGLETON, it may be - in
  ;; effect - impossible to define an initialization method onto the
  ;; singleton instance itself, without first defining the singleton's
  ;; class as a forward-referenced class. Once the class is defined, any
  ;; further initialization methods may be unused.

  ;; FROB - NB affects all DYNAMIC-ENUM (FIXME)
  (defmethod shared-initialize :after ((instance dynamic-enum) slots
                                       &rest initargs &key &allow-other-keys)
    (declare (ignore initargs))
    (when (or (eq slots t)
              (and (consp slots)
                   (find '%members slots :test #'eq)))
      (unless (slot-boundp instance '%members)
        (setf (slot-value instance '%members)
              (make-array 8 :fill-pointer 0
                          :adjustable t)))))

  ;; (shared-initialize (find-class 'trivial-enum) t)

;;(macroexpand-1 (quote

  (defsingleton trivial-enum (simple-dynamic-enum)
    ()
    #+TD (:member-key-test . eq)
    ;; FIXME/API Design - The following two slots, in effect, will limit
    ;; the possible applications of this protocol to the domain of CLOS
    ;; standard objects.
    (:member-class . tenum-member)
    (:member-key-slot . name)
    #+TD (:conc-suffix . #:-tem)
    )

;; (class-finalized-p (find-class 'trivial-enum))
;; => T

;;))

  ;; FIXME: Initialize %members slot for TRIVIAL-ENUM
  ;; (cannot be done via shared-initialize, in this configuration)

  ;; NB:
  ;; (typep (find-class 'trivial-enum) 'dynamic-enum)
  ;; => T


  ;; (enum-members (find-class 'trivial-enum))
  ;;  => NIL ;; supported now, subsequent of the DEFSINGLETON update



  (defun* position-of-tem (name &optional (not-found :error))
    ;; NB: This function should operate onto a recursive read/write
    ;; lock - such that may already be held for :write when this
    ;; function is reached. If the lock is not already held for :write
    ;; then it should be captured for :read within this function
    (declare (type name name)
             (type not-found-symbol not-found)
             (values (or array-dim null) &optional))
    (let* ((whence (find-class 'trivial-enum))
           (buf (%storage-context-members whence))
           (n (position name (the vector buf)
                        :key #'tenum-member-name)))
      (declare (dynamic-extent whence))
      (cond
        (n (values n))
        (not-found
         (ecase not-found
           (:error (error "No item found for name ~s in ~s"
                          name whence))
           ((:ignore nil) (values nil))))
        (t (values nil)))))


  (defun* find-tem (name &optional (not-found :error))
    ;; NB: This function should operate onto a recursive read/write
    ;; lock - such that may already be held for :write when this
    ;; function is reached. If the lock is not already held for :write
    ;; then it should be captured for :read within this function
    (declare (type name name)
             (type not-found-symbol not-found)
             (values (or tenum-member null) boolean
                     &optional))
    (let* ((whence (find-class 'trivial-enum))
           (buf (%storage-context-members whence))
           (n (position-of-tem name not-found)))
      (declare (dynamic-extent whence))
      (cond
        (n (values (aref (the vector buf) n)
                   t))
        (not-found
         (ecase not-found
           (:error (error "No item found for name ~s in ~s"
                          name whence))
           (:ignore (values nil nil))))
        (t (values nil nil)))))


  (defun* register-tem-1 (obj &optional
                              (if-exists :error))
    ;; NB: This function should operate onto a recursive read/write
    ;; lock, ensuring that the lock is held for :write throughout this
    ;; and any local, dispatching function - including POSITION-OF-TEM
    (declare (type tenum-member obj)
             (if-exists-symbol if-exists)
             (values (or tenum-member null) &optional))
    (let* ((whence (find-class 'trivial-enum))
           (buf (%storage-context-members whence)))
      (declare (dynamic-extent whence))
      (labels ((dispatch-reg-new ()
                 ;; FIXME When Debug - Emit a signal here
                 (vector-push-extend obj buf)
                 (values obj))
               (dispatch-override (n)
                 ;; FIXME When Debug - Emit a signal here
                 (setf (aref buf n) obj)
                 (values obj))
               (dispatch-ignore ()
                 ;; FIXME When Debug - Emit a signal here
                 (values nil)))
        (let* ((name (tenum-member-name obj))
               (n (position-of-tem name nil))
               (mem (when n
                      (aref buf n))))
          (cond
            ((and n (not (eq mem obj)))
             (ecase if-exists
               (:error
                ;; NB: Could use CERROR
                (error "~<Attempted to register duplicate item ~s~>
~< for name ~s~>~< as registered for ~s~>~< in ~s~>"
                       obj name mem whence))
               (:override (dispatch-override n))
               ((:ignore nil)
                ;; no-op - other obj MEM already registered
                (dispatch-ignore))))
            (n (dispatch-ignore)) ;; no-op - OBJ already registered
            (t (dispatch-reg-new)))))))


    (defun register-tem (name &optional (if-exists :error))
      ;; NB: hold a recursive write lock for WHENCE

      ;; (declare ...)

      ;; NB IF-EXISTS semantics - :OVERRIDE :ERROR :IGNORE
      (labels ((dispatch-reg ()
                 (let ((obj (make-tem name)))
                   (register-tem-1 obj :override))))
        (let* ((whence (find-class 'trivial-enum))
               (n (position-of-tem name nil)))
          (declare (dynamic-extent whence))
          (cond
            (n
             (ecase if-exists
               (:error
                (error "~<Attempted to register duplicate item~>
~< for name ~s~>~< in ~s~>" name whence))
               ((:ignore nil) (values nil))
               (:override
                (dispatch-reg))))
            (t (dispatch-reg))))))



    (defun remove-tem-1 (obj &optional (not-found :error) no-check)
      ;; NB: hold a recursive write lock for WHENCE

      ;; (declare ...)

      ;; NB Shared NOT-FOUND semantics w/ REMOVE-TEM
      (let* ((whence (find-class 'trivial-enum))
             (buf (%storage-context-members whence)))
        (declare (dynamic-extent whence))
        (labels ((dispatch-remove ()
                   (delete obj (the vector buf)
                           :test #'eq)))
          (cond
            (no-check (dispatch-remove))
            (t
             (ecase not-found
               (:error
                (let ((%obj (find obj buf :test #'eq)))
                  (cond
                    (%obj (dispatch-remove))
                    (t (error "No item ~S found in ~S" obj whence)))))
               ((:ignore nil) (dispatch-remove))))))))


    (defun remove-tem (name &optional (not-found :error))
      ;; NB: hold a recursive write lock for WHENCE

      ;; (declare ...)

      ;; NB NOT-FOUND semantics - ::ERROR IGNORE NIL
      (labels ((dispatch-remove (obj)
                 (remove-tem-1 obj nil t)))
        (multiple-value-bind (%obj foundp)
            (find-tem name not-found)
          (cond
            (foundp (dispatch-remove %obj))
            (t (values nil))))))

    ;; T.D: enum-proto-test.lisp & AFFTA


(eval-when ()
    (defparameter *t1* (register-tem :tem-1))
    (eq (tenum-member-name *t1*) :tem-1)
    (eq (find-tem :tem-1) *t1*)

    (defparameter *t2* (register-tem :tem-2))
    (eq (find-tem :tem-2) *t2*)

    (remove-tem :tem-2)
    (remove-tem :tem-2 :ignore)

    (eq (find-tem :tem-1) *t1*)

    (defparameter *t3* (register-tem :tem-1))
;; .. err

    (eq (find-tem :tem-1) *t1*)
;; => T

    (defparameter *t3* (register-tem-1 (make-tem :tem-1)
                                       :override))

    (eq (find-tem :tem-1) *t3*)

    (%storage-context-members (find-class 'trivial-enum))
)


;;    )
