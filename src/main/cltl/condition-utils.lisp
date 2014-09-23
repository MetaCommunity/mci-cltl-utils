

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

