;; ext-star.lisp

(in-package #:utils.asdf)

(defclass component* (component)
  ())

(defclass module* (component* module)
  ())

(defclass system* (component* system)
  ;; cf. :CLASS initarg to DEFSYSTEM
  ())

(defvar *component*)
(defvar *operation*)

(defmethod operate :around ((operation operation)
                            (component component*)
                            &key &allow-other-keys)
  ;; make OPERATION and COMPONENT avaialble to more specialized methods
  (let ((*component* component)
        (*operation* operation))
    (call-next-method)))
