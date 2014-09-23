

;; depends-on : condition-utils

(in-package #:utils)


(deftype component-designator ()
  '(or string symbol component))

;;; * Component Condition Types

(define-condition component-condition ()
  ((component
    :initarg :component
    :accessor component-condition-component)))

(define-condition component-container-condition ()
  ((container
    :initarg :container
    :accessor component-container-condition-container)))

(define-condition module-component-absent (program-error
					   component-condition
					   component-container-condition)
  ()
  (:report (lambda (c s)
	     (format s "Module ~S does not contain a component ~S"
		     (component-container-condition-container c)
		     (component-condition-component c)))))

;; (error 'module-component-absent :component "FOO" :container "BAR")

(define-condition system-not-found (program-error
				    component-condition
				    component-container-condition)
  ()
  (:default-initargs :container asdf::*source-registry*)
  (:report (lambda (c s)
	     (format s "Source registry ~S does not contain a system ~S"
		     (component-container-condition-container c)
		     (component-condition-component c)))))


;;; * Interfaces onto ASDF Component Search

(defgeneric find-component* (component context &optional errorp)
  ;; FIXME: The order of arguments in this function
  ;; differs substantially from that in ASDF:FIND-COMPONENT
  (:method (system (context null) &optional (errorp t))
    (let ((s (find-system system nil)))
      (cond
	(s (values s))
	(errorp (error 'system-not-found :component system))
	(t (values nil)))))
  (:method (component (context module) &optional (errorp t))
    (let ((c  (find-component context component)))
      (cond
	(c (values c))
	(errorp (error 'module-component-absent
		       :container context
		       :component component))
	(t (values nil))))))

;; (find-component* '#:foobarquux nil)

(defun find-system* (name &optional (errorp t))
  (declare (type component-designator name)
	   (values (or component null)))
  (values (find-component* name nil errorp)))

;; (find-system* "mcclim")


;;; * RESOURCE-SYSTEM


(defclass resource (component) 
  ())

(defclass resource-file (resource static-file)
  ())

(defclass resource-module (resource module)
  ()
  (:default-initargs
   :default-component-class 'resource))

(defclass resource-system (resource system)
  ())

