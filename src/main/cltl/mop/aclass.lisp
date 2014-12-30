;; aclass.lisp - definition of associative-class

(in-package #:info.metacommunity.cltl.utils.mop)


(defgeneric class-object-table-lock (class))
(defgeneric (setf class-object-table-lock) (new-value class))

(defgeneric class-object-table (class))
(defgeneric (setf class-object-table) (new-value class))

(defgeneric class-object-key-slot (class))
(defgeneric (setf class-object-key-slot) (new-value class))

(defclass associative-class (standard-class)
  ((object-table-lock
    :initarg :object-table-lock
    ;; :type lock ;; TO DO : Define this type in bordeaux-threads
    :accessor class-object-table-lock)
   (object-table
    :initarg :object-table
    :accessor class-object-table
    :type hash-table
    :initform (make-hash-table :test 'eq))
   (key-slot
    :initarg :key-slot
    :accessor class-object-key-slot
    :type symbol
    )))

(validate-class associative-class)

(defmethod shared-initialize :after ((instance associative-class)
                                     slots &rest initargs)
  (declare (ignore initargs))
  (when (or (eq slots t)
            (find 'object-table-lock (the list slots) :test #'eq))
    (setf (class-object-table-lock instance)
          (make-lock (format nil "Object-Table Lock (~S)"
                             (or (ignore-errors (class-name instance))
                                 instance))))))

(defmethod finalize-inheritance :after ((class associative-class))
  (let ((ks (class-object-key-slot class))
        (sl (class-slots class)))
    (unless (find ks (the list sl)
                  :key #'slot-definition-name
                  :test #'eq )
      (simple-style-warning  "Key slot ~S not defined to class ~S" 
                             sl class))))

(defgeneric register-object (object class)
  (:method ((object standard-object) (class associative-class))
    (with-lock-held ((class-object-table-lock class))
      (let* ((ks (class-object-key-slot class))
             (key (slot-value object ks))
             (table (class-object-table class))
             (existing (gethash key table)))
        (when existing
          (warn 'redefinition-condition 
                :new object
                :previous existing))
        (setf (gethash key table) object)
        (values object key)))))

(defgeneric find-object (name class &optional errorp)
  (:method (name (class associative-class) &optional (errorp t))
    (with-lock-held ((class-object-table-lock class))
      (let ((table (class-object-table class)))
        (multiple-value-bind (instance foundp)
            (gethash name table)
          (cond
            (foundp (values instance name))
            (errorp (error 'entity-not-found :name name))
            (t (values nil nil))))))))

(defgeneric remove-object (name class)
  (:method (name (class associative-class))
    (with-lock-held ((class-object-table-lock class))
      (let* ((table (class-object-table class))
             (exp (remhash name table)))
        (values exp name)))))

(defgeneric map-objects (function class)
  (:method (function (class associative-class))
    (map-objects (coerce function 'function) class))
  (:method ((function function) (class associative-class))
    (with-lock-held ((class-object-table-lock class))
      (let ((table (class-object-table class)))
        (labels ((fncall (name object)
                   (declare (ignore name))
                   (funcall function object)))
          (maphash #'fncall table)))
      (values class))))


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
  

|#
