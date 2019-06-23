;; spec-mop.lisp - a demonstration about standard MOP method calls

(in-package #:ltp/common/mop)

(defgeneric frob-call (a b c))

(macrolet ((mk-specialization (types)
             (let ((vardecls (mapcar #'(lambda (typ)
                                         (list (gensym (symbol-name typ))
                                               typ))
                                     types)))
               `(defmethod frob-call (,@vardecls)
                  (format *debug-io* "~%FROB-CALL (~s ~s ~s) : ~s ~s ~s"
                          ,@(mapcar #'(lambda (typ) `(quote ,typ)) types)
                          ,@(mapcar #'car vardecls))
                  (values (quote ,types))))))

    (mk-specialization (array fixnum t))
    (mk-specialization (string integer t))
    (mk-specialization (string fixnum t))
    (mk-specialization (array integer t))
    (mk-specialization (t fixnum t))
    (mk-specialization (t fixnum cons))
    (mk-specialization (t integer t))
    (mk-specialization (t t symbol))
    (mk-specialization (array t t))
    (mk-specialization (string t t))
    (mk-specialization (t t t))
    (mk-specialization (t t list))

    (mk-specialization (integer t t))
    (mk-specialization (fixnum t t)))


(frob-call "Frob" 5 nil)
;; => (STRING FIXNUM T)
(frob-call #() 5 nil)
;; => (ARRAY FIXNUM T)
(frob-call 5 5 '(a b c))
;; => (FIXNUM T T)


;; NB: The consing - a generalization of &REST ??
(compute-applicable-methods #'frob-call '("Frob" 5 nil))
;; ^ it matches the specilization in spec.lisp

(compute-applicable-methods #'frob-call '(#() 5 nil))
;; ^ same

(compute-applicable-methods #'frob-call '(5 5 '(a b c)))
;;
