
(in-package #:cl-user)

(defclass model-class (standard-class)
  ;; TBD: Model [property]
  ())

(defclass abstract-model-class (model-class)
  ())


(defclass element (#+NIL metaclass)
  ;; protocol class - cf. UML
  ;; see e.g http://www.uml-diagrams.org/namespace.html
  ()
  (:metaclass abstract-model-class))

(defclass named-element (element)
  ((name
    :initarg :name
    :type simple-string
    :accessor named-element-name))
  (:metaclass abstract-model-class))

(defclass namespace (named-element)
  ()
  (:metaclass abstract-model-class))
