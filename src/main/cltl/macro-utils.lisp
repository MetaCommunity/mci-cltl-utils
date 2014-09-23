
(in-package #:utils)

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
