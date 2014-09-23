
(in-package #:utils)


(defmacro call-next-method* (&rest args)
  (with-gensym (%nmp)
    `(let ((,%nmp (next-method-p)))
       (cond
	 (,%nmp (call-next-method ,@args))
	 (t (values nil nil))))))
