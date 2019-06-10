
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

(defgeneric %enum-members (enum))
(defgeneric (setf %enum-members) (new enum))

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
  ((%members
    :initarg :%members
    :accessor %enum-members ;; see also: :SYNCHRONIZE (NB)
    ;; :synchronize (:read :write)
    :type (array t (*)))
   (member-register-function
    ;; :access :read-only
    ;; may be unbound
    :initarg :member-register-function
    :type function)
    (member-remove-function
    ;; :access :read-only
    ;; may be unbound
    :initarg :member-remove-function
    :type function)))



(defmethod shared-initialize ((instance dynamic-enum) slots
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
  (:method ((enum dynamic-enum))
    (when (slot-boundp enum '%members)
      (coerce (the (array t (*)) (%enum-members enum))
              'list))))

(defgeneric (setf enum-members) (new-value enum)
  (:method ((new-value list) (enum dynamic-enum))
    (setf (enum-members enum)
          (coerce (the list new-value) 'simple-vector)))
  (:method ((new-value vector) (enum dynamic-enum))
    (cond
      ((array-has-fill-pointer-p new-value)
       (setf (%enum-members enum) new-value))
      (t
       (let ((len (length new-value)))
         (setf (%enum-members enum)
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

  (%enum-members *enum*)

  (vector-push-extend "E" (%enum-members *enum*))

  (enum-members *enum*)

  (setf (enum-members *enum*)
        (reverse (enum-members *enum*)))

  (enum-members *enum*)

  (let* ((buf (%enum-members *enum*))
         (n (length buf))
         (newbuf
          (make-array n
                      :element-type t
                      :fill-pointer n
                      :initial-contents (nreverse buf))))
    (values
     (setf (enum-members *enum*) newbuf)
     (eq (%enum-members *enum*) newbuf)))
  ;; last value => T


)



;; ----

(defsingleton dynamic-enum-class (dynamic-enum)
  ())


;; --

(defclass static-enum (enum)
  ((%members
    :type simple-vector
    ;; :access :read-only ;; TD
    :reader %enum-members)))


(defmethod shared-initialize ((instance static-enum) slots
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

(defmethod enum-members ((enum static-enum))
  (when (slot-boundp enum '%members)
    (coerce (the simple-vector (%enum-members enum))
            'list)))

;; TD: Instance tests - STATIC-ENUM Initialization, Accessors



;; ----

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
(eval-when () ;; Basic prototyping/Proof of concept

  (deftype name ()
    'symbol)

  (defclass tenum-member ()
    ;; NB IMPL: DEFSINGLETON ( & ENUM KEY INITARG)
    ((name
      :initarg :name
      :type name
      :reader tenum-member-name)))

  (deftype if-exists-symbol ()
    '(member :override :error :ignore))

  (deftype not-found-symbol ()
    '(member :error :ignore))

  (defsingleton trivial-enum (dynamic-enum)
    ()
    (:member-class . tenum-member)
    (:member-key-slot . name))
  ;; FIXME: How is it not finalized?

  #+NIL
  (slot-value (find-class 'trivial-enum)
              'member-class)
  ;; ^ is correctly bound.

  ;; (enum-members (find-class 'trivial-enum))
  ;;  => NIL ;; supported now, subsequent of the DEFSINGLETON update

  ;; FIXME: Do not err when the %MEMBERS slot is unbound, in the
  ;; following

  (defun find-tem (name &optional (not-found :error))
    (let ((buf (%enum-members (find-class 'trivial-enum))))
      (cond
        (t (values nil nil)))))

  (defun register-tem-1 (obj &optional (if-exists :error))
    (let ((buf '#:...))
    ;; TBD Shared IF-EXISTS semantics w/ REGISTER-TEM
      ))

  (defun register-tem (key &optional (if-exists :error))
    ;; TBD IF-EXISTS semantics - :OVERRIDE :ERROR IGNORE
    (let ((obj '#:...))
      (register-tem-1 obj)))


  (defun remove-tem-1 (obj &optional (not-found :error))
    ;; TBD Shared NOT-FOUND semantics w/ REMOVE-TEM
    (let ((buf '#:...))
      ))

  (defun remove-tem (name &optional (not-found :error))
    ;; TBD NOT-FOUND semantics - ::ERROR IGNORE
    (let ((buf '#:...))
      ))

)
