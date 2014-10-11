
(in-package #:utils)


(defmacro call-next-method* (&rest args)
  (with-gensym (%nmp)
    `(let ((,%nmp (next-method-p)))
       (cond
	 (,%nmp (call-next-method ,@args))
	 (t (values nil nil))))))

(defmacro slot-value* (object slot &optional default)
  ;; NOTE: This is defined as a macro, so as to allow the compiler to
  ;; optimize the SLOT-VALUE call, if applicable
  (with-gensym (o s)
    `(let ((,o ,object)
           (,s ,slot))
       (cond
         ((slot-boundp ,o ,s)
          (values (slot-value ,o ,s)
                  t))
         (t (values nil nil))))))
         
