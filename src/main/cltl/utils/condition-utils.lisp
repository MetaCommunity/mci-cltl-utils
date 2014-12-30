

(in-package #:utils)

(define-condition simple-style-warning (style-warning simple-condition)
  ())

(defmacro simple-style-warning (fmt-ctrl &rest args)
  `(warn 'simple-style-warning 
	 :format-control ,fmt-ctrl
	 :format-arguments (list ,@args)))


(define-condition simple-program-error (program-error simple-condition)
  ())

(defmacro simple-program-error (fmt-ctrl &rest args)
  `(error 'simple-program-error
	  :format-control ,fmt-ctrl
	  :format-arguments (list ,@args)))


(defgeneric format-condition (condition stream)
  (:documentation "CLOS Utility for application within condition class REPORT forms")
  (:method ((condition condition) (stream symbol))
    (format-condition condition
                      (ecase stream
                        ((t) *terminal-io*)
                        ((nil) *standard-output*))))
  (:method ((condition simple-condition) (stream stream))
    (format stream (simple-condition-format-control condition)
            (simple-condition-format-arguments condition))))

(define-condition entity-condition ()
  ((name
    :initarg :name
    :reader entity-condition-name)))

(define-condition entity-not-found (error entity-condition)
  ()
  (:report format-condition))

(defmethod format-condition ((c entity-not-found) (s stream))
  (format s "Not found: ~S"
          (entity-condition-name c)))

(define-condition redefinition-condition (style-warning)
  ((previous-object
    :initarg :previous
    :accessor redefinition-condition-previous-object)
   (new-object
    :initarg :new
    :accessor redefinition-condition-new-object))
  (:report format-condition))

(defmethod format-condition ((c redefinition-condition) (s stream))
  (format s "Redefinition ~<~S~> => ~<~S~>"
          (redefinition-condition-previous-object c)
          (redefinition-condition-new-object c)))

(define-condition container-condition ()
  ((container
    :initarg :container
    :reader container-condition-container)))

