
(in-package #:ltp/common)

(defpackage #:ltp/common/mop/enum
  (:use #:ltp/common/mop/singleton
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
    (coerce (the (array t (*)) (%enum-members enum))
            'list)))

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
;; ^ FIXME limit FTYPE decls onto defined methods, once declared

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
  (coerce (the simple-vector (%enum-members enum))
          'list))

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
  ;; - Params: Refer to direct slot definitions of the classes
  ;;   ENUM and DYNAMIC-ENUM. Excepting the slot definition for the
  ;;   %MEMBERS slot, those class direct slots' other initialization
  ;;   aruments may be provided via the PARAMS section of this macro.

  (labels ((get-one-param (%name %params)
             (declare (type symbol name) (type list params))
             (let ((n (position %name %params
                                :key #'car
                                :test #'eq)))
               (cond
                 (n (let ((p (nth n params)))
                      (values (cdr p)
                              t
                              (remove p %params :test #'eq))))
                 (t (values nil nil params)))))

           (mk-default-name (prefix typ)
             ;; FIXME: Consider providing some manner of a formatter
             ;; parameter for customizing the syntax of SINGLETON ENUM
             ;; accessor function names.
             (intern (concatenate 'simple-string
                                  (symbol-name prefix)
                                  "-"
                                  (symbol-name typ)))))

    (multiple-value-bind (reg-form regp params)
        (get-one-param :register-function params)

      (unless regp
        (setq reg-form (mk-default-name '#:register name)))

      (cond
        ((and regp (listp reg-form) (listp (cadr reg-form))))
        ;; ^ no-op - interpret value as an implicit defun form
        ((and regp (null reg-form)))
        ;; ^ no-op - do not define or store a :register-function
        (reg-form
         ;; ^ function name

         ;; create implicit defun (??) [TBD] (NB At least, store ref to
         ;; fn - may be forward-referenced) (??)

         ;; FIXME - consider evaluating (COMPUTE-DEFAULT-REGISTRATION-LAMBDA...)
         ;; in the compiler environment -- assuming that the class denoted by
         ;; NAME will have been finalized by the time that that form is
         ;; evaluated

         #+TD
         (let ((reg-lambda (compute-default-registration-lambda ...)))
           (setq reg-form `(,name ,@(cdr reg-lamba))))))


      (multiple-value-bind (find-form findp params)
          (get-one-param :find-function params)
        (unless findp
          (setq find-form (mk-default-name '#:find name)))

        ;; (compute-default-find-lambda ...)

        (multiple-value-bind (remove-form remp params)
            (get-one-param :remove-function params)
          (unless remp
            (setq remove-form (mk-default-name '#:remove name)))

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
             )))))))


