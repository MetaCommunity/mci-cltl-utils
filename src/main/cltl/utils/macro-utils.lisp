
(in-package #:mcicl.utils)

(defmacro format* (ctrl &rest args)
  `(format nil ,ctrl ,@args))


(defmacro with-gensym ((&rest names) &body body)
  `(let (,@(mapcar (lambda (name)
		     (let ((tmp (gensym (format* "%~A-" name))))
		       `(,name (quote ,tmp))))
		   names))
     ,@body))

;; (macroexpand-1 '(with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4))))
;; (with-gensym (a b) `(let ((,a 1) (,b 2)) (list ,a ,b 3 4)))


(defmacro intern* (s &optional (package *package*))
  `(intern (symbol-name ,s)
	   ,package))

;; (intern* (quote #:foo))


(defmacro intern-formatted (ctrl &rest args)
  `(intern* (read-from-string (format* ,ctrl ,@args))))

;; (intern-formatted "#:g-~a-foo" 'widget)
		   
