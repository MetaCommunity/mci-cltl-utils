;; aclass.lisp - protocol for generic tabular indexing onto CLOS

(in-package #:mcicl.mop)


;;; % Associative Index

(defgeneric object-table-lock (index))
(defgeneric (setf object-table-lock) (new-value index))

(defgeneric object-table (index))
(defgeneric (setf object-table) (new-value index))

(defgeneric object-key-slot (index))
(defgeneric (setf object-key-slot) (new-value index))

(defclass associative-index ()
  ((object-table-lock
    :initarg :object-table-lock
    ;; :type lock ;; FIXME: Define type `LOCK' in bordeaux-threads
    :accessor object-table-lock)
   (object-table
    ;; FIXME: This slot should be defined in a subclass,
    ;;  as to not require that every ASSOCIATIVE-INDEX would store a
    ;;  HASH-TABLE
    ;;
    ;; e.g within (DEFCLASS TABULAR-INDEX ...)
    :initarg :object-table
    :accessor object-table
    :type hash-table
    :initform (make-hash-table :test 'eq))
   ))


(defmethod shared-initialize :after ((instance associative-index)
                                     slots &rest initargs)
  (declare (ignore initargs))
  (when (or (eq slots t)
            (find 'object-table-lock (the list slots) :test #'eq))
    (setf (object-table-lock instance)
          (make-lock (format nil "Object-Table Lock (~S)"
                             (or (ignore-errors (class-name instance))
                                 instance))))))


;;; % Simple Associative Index

(defgeneric object-table-key-function (index))
(defgeneric (setf object-table-key-function) (new-value index))

(defclass simple-associative-index (associative-index)
  ((key-function
    :initarg :key-function
    :type function
    :accessor object-table-key-function)))


(defmethod shared-initialize :around ((instance simple-associative-index)
                                      slots &rest initargs
                                      &key (key-function nil kfp))
  (let (args-changed-p)
    (when (and kfp
               (not (typep key-function 'function)))
      (setf args-changed-p t
            (getf initargs :key-function)
            (coerce key-function 'function)))
    (cond
      (args-changed-p
       (apply #'call-next-method instance slots initargs))
      (t (call-next-method)))))


;;; % Index Protocol

(defgeneric compute-key (object index)
  (:method (object (index simple-associative-index))
    (funcall (the function (object-table-key-function index))
             object)))


(defgeneric register-object (object index)
  (:method ((object standard-object) (index associative-index))
    (with-lock-held ((object-table-lock index))
      (let* ((key (compute-key object index))
             (table (object-table index))
             (existing (gethash key table)))
        (when existing
          (warn 'redefinition-condition
                :new object
                :previous existing))
        (setf (gethash key table) object)
        (values object key)))))

(defgeneric find-object (name index &optional errorp)
  (:method (name (index associative-index) &optional (errorp t))
    (with-lock-held ((object-table-lock index))
      (let ((table (object-table index)))
        (multiple-value-bind (instance foundp)
            (gethash name table)
          (cond
            (foundp (values instance name))
            (errorp (error 'entity-not-found :name name))
            (t (values nil nil))))))))

(defgeneric remove-object (name index)
  (:method (name (index associative-index))
    (with-lock-held ((object-table-lock index))
      (let* ((table (object-table index))
             (exp (remhash name table)))
        (values exp name)))))

(defgeneric map-objects (function index)
  (:method (function (index associative-index))
    (map-objects (coerce function 'function) index))
  (:method ((function function) (index associative-index))
    (with-lock-held ((object-table-lock index))
      (let ((table (object-table index)))
        (labels ((fncall (name object)
                   (declare (ignore name))
                   (funcall function object)))
          (maphash #'fncall table)))
      (values index))))


;;; % Associative Class

(defclass associative-class (associative-index standard-class)
  ((key-slot
    :initarg :key-slot
    :accessor object-key-slot
    :type symbol
    )))

(validate-class associative-class)


(defmethod finalize-inheritance :after ((class associative-class))
  (let ((ks (object-key-slot class))
        (sl (class-slots class)))
    (unless (find ks (the list sl)
                  :key #'slot-definition-name
                  :test #'eq )
      (simple-style-warning  "Key slot ~S not defined to class ~S"
                             sl class))))

(defmethod compute-key ((object standard-object) (index associative-class))
  (slot-value object (object-key-slot index)))

;; (defgeneric remove-object ...)

#| Instance tests

(defclass afoo ()
  ((fie :initarg :fie :type symbol)
   (fum :initarg :fum))
  (:metaclass associative-class)
  (:key-slot . fie))


;; test REGISTER-OBJECT / FIND-OBJECT ...
(let ((af1 (make-instance 'afoo :fie '|1| :fum "One"))
      (af2 (make-instance 'afoo :fie '|2| :fum "Two"))
      (c (find-class 'afoo))
      #+NIL (af3 (make-instance 'afoo)))
  (register-object af1 c)
  (register-object af2 c)
  (labels ((frob-test (k)
             (let ((o (find-object k c)))
               (eq (slot-value o 'fie) k))))
    (values (frob-test '|1|)
            (frob-test '|2|))))
;;; => T, T

;; FIXME: Define more tests - application of AFFTA

|#
