

;; depends-on : condition-utils

(in-package #:utils)


(deftype component-designator ()
  '(or string symbol component))

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
	     (format s "Module ~A does not contain a component ~A"
		     (component-container-condition-container c)
		     (component-condition-component c)))))

;; (error 'module-component-absent :component "FOO" :container "BAR")

(define-condition system-not-found (program-error
				    component-condition
				    component-container-condition)
  ()
  (:default-initargs :container asdf::*source-registry*)
  (:report (lambda (c s)
	     (format s "Source registry ~A does not contain a system ~S"
		     (component-container-condition-container c)
		     (component-condition-component c)))))


(defgeneric find-component* (component context &optional errorp)
  (:method ((component symbol) context &optional (errorp t))
    (find-component* (coerce-name component) context errorp))
  (:method (component (context symbol) &optional (errorp t))
    (find-component* component (coerce-name context) errorp))
  (:method ((system string) (context null) &optional (errorp t))
    (let ((s (find-system system nil)))
      (cond
	(s (values s))
	(errorp (error 'system-not-found :component system))
	(t (values nil)))))
  (:method ((component component) (context module) &optional (errorp t))
    (let ((c  (find-component component context)))
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
  (find-component* name nil errorp))

;; (find-system* "mcclim")


