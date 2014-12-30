;; mop-utils.lisp - utilities onto MOP implementations

(in-package #:info.metacommunity.cltl.utils.mop)


(defmacro validate-class (class &optional (superclass 'standard-class))
  #+(or SBCL CMU CCL ALLEGRO)
  `(progn 
     (defmethod validate-superclass ((a ,class) (b ,superclass))
       (values t))
     (defmethod validate-superclass ((a ,superclass) (b ,class))
       (values t)))
  #-(or SBCL CMU CCL ALLEGRO)
  `(values))
