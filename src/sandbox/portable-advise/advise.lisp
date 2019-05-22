;; advise.lisp - portable reflective value wrapping for Common Lisp functions


(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
(defpackage #:reflexion
  (:use
   ;; #:libfoo
   #:cl
   )

  (:export
   #:defadvise
   #:unadvise
   ))) ;; EVAL-WHEN


(in-package #:reflexion)


(deftype function-name ()
  ;; also defined in libfoo
  '(or symbol (cons symbol t)))

;; trivial portable prototype of an 'advise' interface

(defstruct (advise-record
             (:conc-name #:advised-)
             (:constructor
              make-advise-record (name kind env original-function)))
  (original-function
   #'(lambda ()
       (error "Default advised-original-function"))
   :type function)
  ;; NB: no portable 'environment' type.
  ;; 'env' slot may only be useful for debugging
  (kind 'defun :type (member defun defmacro))
  (env nil)
  (name #:unnamed
        :type function-name))

(define-condition advise-condition ()
  ((name :initarg :name
         :reader advise-condition-name)))

(define-condition already-advised (program-error advise-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Already advised: ~s" (advise-condition-name c)))))

(define-condition not-advised (program-error advise-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Not advised: ~s" (advise-condition-name c)))))


(defconstant %advise-expand% 8)

(defconstant %advise-unbound%
  (cond
    ((boundp '%advise-unbound%)
     (symbol-value '%advise-unbound%))
    ;; NB: Non-sandard : ALLOCATE-INSTANCE on STRUCTURE-CLASS
    (t (let ((inst (allocate-instance (find-class 'advise-record))))
         (setf (advised-name inst)
               (gensym "%unbound-"))))))
                                       

(defvar %advise-rec%
  (make-array %advise-expand%
              :element-type 'advise-record
              :adjustable t
              :initial-element %advise-unbound%
              :fill-pointer 0))

(defun get-advise (name env &optional (errorp t))
  (declare (type function-name name)
           (ignore env) ;; FIXME
           (values (or null advise-record)))
  ;; FIXME: lock %ADVISE-REC% (READ, NON-RECURSIVE) in this function
  (labels ((test (a b)
             (etypecase a
               (symbol (and (symbolp b)
                            (eq a b)))
               (cons (and (consp b)
                          (test (car a) (car b))
                          (test (cdr a) (cdr b)))))))
    ;; (test '(a a) '(a b)) ;; => NIL
    ;; (test '(a a) '(a a)) ;; => T
    ;; (test '(a a a) '(a a b)) ;; => NIL
    ;; (test '(a a b) '(a a b)) ;; => T
    (let ((o (find name %advise-rec%
                   :test (etypecase name
                           (symbol #'eq)
                           (cons #'test))
                   :key #'advised-name)))
      (cond
        (o (values o))
        (errorp (error 'not-advised :name name))
        (t (values nil nil))))))

(defun register-advise (name kind env form)
  ;; FIXME: lock %ADVISE-REC% (WRITE) in this function
  (let ((adv (get-advise name env nil)))
    (cond
      ((and adv
            (not (eq form (advised-original-function adv))))
       (error 'already-advised :name name))
      (t (let ((adv (make-advise-record name kind env form)))
           (vector-push-extend adv %advise-rec% %advise-expand%)
           (values adv))))))

(defun unregister-advise (name env)
  ;; FIXME: lock %ADVISE-REC% (WRITE) in this function
  (let ((adv (get-advise name env nil)))
    (cond
      (adv
       (setf %advise-rec% (delete adv %advise-rec% :test #'eq))
       (values adv))
      (t (error 'not-advised :name name)))))

  

(defmacro defadvise (name lambda &key before after &environment env)
  ;; FIXME: This does not destructure LAMBDA for the BEFORE, AFTER functions,
  ;;        and neither for the call to the advised macro function.

  (let ((%name (gensym "%name-"))
        (%env (gensym "%env-"))
        (%form (gensym "%form-"))
        (%kind (gensym "%kind-"))
        )

    ;; FIXME: Difficuly to properly deconstruct DEFMETHOD LAMBDA for
    ;; bindings in the BEFORE, AFTER functions

    ;; see also: alexandria.0.dev:parse-ordinary-lambda-list
    
    `(let* ((,%name (quote ,name))
            (,%env ,env)
            (,%kind 'defun)
            (,%form
             (or (let ((,%form (macro-function ,%name ,%env)))
                   ;; FIXME: THIS might not always "work out"
                   (when ,%form
                     (setq ,%kind 'defmacro)
                     (eval (function-lambda-expression ,%form))))
                 (fdefinition ,%name)
                 (error "No macro function and no function defined for name ~S"
                        ,%name))))

       (register-advise ,%name ,%kind ,%env ,%form)

       (macrolet ((do-def ()
                    `(,%kind ,name ,lambda
                       ,@(when before
                               `((funcall (lambda () ,before ))))
                       (unwind-protect
                            , (ecase ,%kind
                                (defmacro
                                    (eval (funcall (eval ,%form)
                                                   ;; FIXME : ,LAMBDA NOT RIGHT HERE
                                                   ?????
                                                   ,%env)))
                                (defun
                                    (funcall ,%form
                                             THE-FANCY-ARGS-LIST)))
                         ,@(when after
                                 `((funcall (lambda () ,after))))))))
         (do-def)))))



(defmacro unadvise (name &environment env)
  (let ((%name (gensym "%name-"))
        (%adv (gensym "%adv-"))
        (%kind (gensym "%kind-"))
        (%fn (gensym "%fn-")))
    `(let* ((,%name (quote ,name))
            (,%adv (unregister-advise ,%name ,env))
            (,%kind (advised-kind ,%adv))
            (,%fn (advised-original-function ,%adv)))
       ;; FIXME: MACRO
       (ecase ,%kind
         (defmacro (setf (macro-function ,%name) ,%fn))
         (defun (setf (fdefinition ,%name) ,%fn)))
       (compile ,%name ,%fn)
       (values ,%fn))))

#|

(defmacro frob-trace (ctrl &rest args)
  `(progn 
     (write-char #\Newline *trace-output*)
     (format *trace-output* ,ctrl ,@args)
     (write-char #\Newline *trace-output*)
     (finish-output *trace-output*)))

(defmacro quux (arg)
  `(progn (frob-trace "GOT ARG QUUX ~S" ,arg)
          (list :quux ,arg)))

#+test (quux 5)

(macroexpand (quote
(defadvise quux (%arg)
  :before (lambda (%argtoo)
            (frob-trace "GOT ARGTOO BEFORE ~s" %argtoo))
  :after  (lambda (%argtoo)
            (frob-trace "GOT ARGTOO AFTER ~s" %argtoo))
  )
))

#+test (quux 12)
;; ^FIXME: *trace-output* shows order of eval: :BEFORE, :AFTER, :DURING

(unadvise quux)

#+test (quux 28)



|#

;; Prototypes



#+NIL
(defmacro compute-defkind (name &environment env)
  ;; endeavor to compute a discrete 'kind' of a function's definition
  ;; 
  ;; NB: This does not compute 'definitino source kind' - e.g.
  ;; whether a function is defined of an accessor, constructor, or
  ;; other non-DEFUN source - such that may serve to require an
  ;; implementation-specific reflection, and may require more than
  ;; only a function's name, to compute.
  (let ((%name (gensym "%name-"))
        (%env (gensym "%env-"))
        (%fn (gensym "%fn-")))
    `(let ((,%name (quote ,name))
           (,%env ,env))
       (declare (type (or symbol (cons symbol t)) ,%name))
       (cond
         ((macro-function ,%name ,%env)
          (values 'defmacro))
         ((and (consp ,%name)
               (eq (car ,%name) 'setf))
          ;; ??
          (values 'defsetf))
         ((and (consp ,%name)
               (eq (car ,%name) 'lambda))
          ;; ??
          (values 'lambda))
         (t
          (let ((,%fn (fdefinition ,%name)))
            (typecase ,%fn
              (generic-function
               (values 'defgeneric))
              (standard-object ;; funcallable-standard-object portably
               ;; ??
               (values 'defclass))
              (function
               (values 'defun))
              (t
               (values :unknown)))))))))
