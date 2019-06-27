;; initfn.lisp - prototypes onto portable &env application

(in-package #:ltp/common)

(defmacro frob-env (&environment env)
  `(values ,env))


(defun mk-initfunction-lambda (form environment)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion form environment)
    (declare (ignore store-vars writer-form))
    `(lambda ()
       (let (,@(when vars
                     (mapcar #'list vars vals)))
         #+NIL
         ,@(when vars `((declare (ignorable ,@vars))))
         ,reader-form))
       ))

(eval-when ()

  ;; NB: The usage of GET-SETF-EXPANSION in MK-INITFUNCTION-LAMBDA may
  ;; not be enough to minimize compiler warnings, if an initfunction is
  ;; compiled in an environment in which any variables referenced in the
  ;; initfunction are not bound.

  ;; NB: SYMBOL-VALUE itself does not provide a portable 'environment' param

  ;; Some functions, "Beyond this," may be implementation-specific,
  ;; moreover application-specific - e.g if to retrieve the value of a
  ;; symbol initform from within the environment in which that iniform
  ;; was originally declared, in evalution -- in precedence to any
  ;; environment in which any resulting initfunction may be evaluated.

  (mk-initfunction-lambda '*standard-output* nil)

  (mk-initfunction-lambda '*standard-output* (frob-env))

  (let ((*standard-output* *debug-io*))
    (mk-initfunction-lambda '*standard-output* (frob-env)))

  (let ((frob 12321))
    (mk-initfunction-lambda 'frob (frob-env)))

  (let ((frob (list 1 2)))
    (mk-initfunction-lambda '(cdr frob) (frob-env)))

  (let ((frob (list 1 2)))
    (mk-initfunction-lambda '(cdr assume-bound) (frob-env)))

  (mk-initfunction-lambda '(cdr assume-bound) (frob-env))


  (let ((frob (list 1 2)))
    (declare (dynamic-extent frob)) ;; no change to the resulting form
    (mk-initfunction-lambda '(cdr frob) (frob-env)))
)
