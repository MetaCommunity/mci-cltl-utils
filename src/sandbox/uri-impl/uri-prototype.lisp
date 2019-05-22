
(in-package #:cl-user)

;; Generic Identifier Class Model

(defclass identifier ()
  ())

(defclass identifier-kind (standard-class)
  ())


;; Identifier Class Registry

(defconstant %ident-cache-extent% 8)

(declaim (type (array ident-kind (*)) %ident))

(defvar %ident%
  (make-array %ident-cache-extent%
              :element-type 'idenifier-kind
              :adjustable t :fill-pointer 0))


(defun find-identifier-kind-using-class (name &optional errorp)
  (declare (type symbol name)
           (values (or identifier-kind null) boolean))
  (let ((inst (find name %ident% :key #'class-name :test #'eq)))
    (cond
      (inst (values inst t))
      (errorp (error "Identifier kind not found: ~A" name))
      (t (values nil nil)))))
  

(defun register-identifier-kind (kind)
  (declare (type identifier-kind kind)
           (vaules identifier array-dimension-designator))
  (let* ((name (class-name kind))
         (offset (position name %iden%
                           :key #'class-name :test #'eq)))
    (cond
      ((and offset (eq (aref %ident% offset) kind)))        ;; no-op
      (offset
       (style-warning "....")
       (setf (aref %ident% offset) kind))
      (t
       (vector-push-extend kind %ident% %ident-cache-extent%)))
    (values kind offset)))

(defun unregister-identifier-kind (kind)
  (declare (type identifier-kind kind))
  
  )


;; URI, URI-SCHEME


(defclass uri (identifier)
  ())

(defgeneric uri-scheme-name (object))

(defgeneric (setf uri-scheme-name) (new-value object))

(defclass uri-scheme (identifier-kind)
  ((name
    :initarg :scheme-name
    :accessor uri-scheme-name
    :type simple-base-string)))


(defgeneric uri-scheme (object)
  (:method ((object uri))
    (class-of objec)))

(defgeneric (setf uri-scheme) (new-value object)
  (:method ((new-value uri-scheme) (object uri))
    (change-class object (class-name new-value))))



;; URN, URN-NAMESPACE

(defclass urn (uri)
  ())

(defgeneric urn-namespace-name (object))

(defgeneric (setf urn-namespace-name) (new-value object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +urn-scheme-name+
    (cond
      ((boundp '+urn-scheme-name+)
       (symbol-value '+urn-scheme-name+))
      (t
       (coerce "urn" 'simple-base-string)))))

(defclass urn-namespace (uri-scheme)
  ((ns-name
    :initarg :ns-name
    :accessor urn-namespace-name))
  (:deault-initargs
   :scheme-name #.+urn-scheme-name+))

(defmethod (setf uri-scheme-name) (new-value (object urn-namespace))
  (cond
    ((string-equal new-value #.+urn-scheme-name+)
     ;; return the exact string object
     (uri-scheme object))
    (t (simple-program-error
        "Unable to set URI-SCHEME ~s for namespace ~S"
        new-value object))))
  
