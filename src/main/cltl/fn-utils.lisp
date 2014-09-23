
(in-package #:utils)

(defmacro with-safe-frefs (specs &body body)
  ;; NB : "Not applicable" for (SETF FOO) function names
  (let ((%s (gentemp "%s-"))
	(%foundp (gentemp "%foundp-")))
    (flet ((s-name (s)
	     (etypecase s
	       (string s)
	       (symbol (symbol-name s))
	       (character (string s))))
	   (pkg-name (p)
	     (etypecase p
	       (package (package-name p))
	       (symbol (symbol-name p))
	       ((or string character)
		(values p)))))
      `(let (,@(mapcar (lambda (spec)
			 (destructuring-bind 
			       (name fn &optional 
				     (package
				      (etypecase fn
					(symbol
					 (or (symbol-package fn)
					     *package*))
					((or string character)
					 *package*))))
			     spec
			   (let ((fname (s-name fn))
				 (pkgname (pkg-name package)))
			     `(,name
				(multiple-value-bind (,%s ,%foundp)
				    (find-symbol ,fname
						 ,pkgname)
				  (cond
				    (,%foundp
				     (values (fdefinition ,%s)))
				    (t
				     (error "Package ~A does not contain a symbol ~S"
					    (find-package ,pkgname)
					    (quote (quote ,fname))))
				    ))))))
		       specs))
	 ,@body))))
