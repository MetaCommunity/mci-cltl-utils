

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
  (:method ((condition condition) (stream symbol))
    (format-condition condition
                      (ecase stream
                        ((t) *terminal-io*)
                        ((nil) *standard-output*))))
  (:method ((condition simple-condition) (stream stream))
    (format stream (simple-condition-format-control condition)
            (simple-condition-format-arguments condition))))
