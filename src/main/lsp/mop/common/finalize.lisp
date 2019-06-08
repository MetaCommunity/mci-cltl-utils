
(in-package #:ltp/common/mop)

;; (defgeneric finalize-reachable-subclass (sub super))

(define-condition class-finalization-condition ()
  ((class
    :initarg :class
    :reader class-finalization-condition-class)))


(define-condition class-finalization-error
    (error class-finalization-condition)
  ())

(define-condition simple-class-finalization-error
    (simple-error class-finalization-error)
  ())

(define-condition class-finalization-warning
    (warning class-finalization-condition)
  ())

(define-condition class-finalization-style-warning
    (style-warning class-finalization-warning)
  ())

(define-condition simple-class-finalization-style-warning
    (simple-style-warning class-finalization-style-warning)
  ())


(eval-when (:compile-toplevel :execute)
  (defmacro class-finalization-note (fmt other &rest rest)
    `(warn 'simple-class-finalization-style-warning
           :class ,other
           :format-control ,fmt
           :format-arguments (list ,other ,@rest))
    (values)))


;; --


;; NB Interface Signatures: Ensure that successful calls to
;;     FINALIZE-REACHABLE... will return one non-nil value


(defgeneric finalize-reachable (instance)
  ;; NB: An additional method onto this generic function is defined in
  ;;     the SINGLETON system, such that will iterate across direct
  ;;     superclasses of the INSTANCE -- finalizing each direct
  ;;     superclass, when not a forward referenced class, or returning
  ;;     no values from the method. That method will then finalize the
  ;;     INSTANCE itself, and will ensure that any rechable subclass of
  ;;     the INSTANCE is finalized.
  (:method ((instance forward-referenced-class))
    (class-finalization-note
     "Cannot finalize foward-referenced class ~S"
     instance))
  (:method ((instance standard-class))
    (labels ((direct-super-fwd-p (c)
               (dolist (%c (class-direct-superclasses c) nil)
                 (when (typep %c 'forward-referenced-class)
                   (return %c)))))
      (let ((fwd (direct-super-fwd-p instance)))
        (cond
          (fwd
           (class-finalization-note
            "~<Cannot finalize class ~S~> ~
~< having forward-referenced direct superclass ~S~>" instance fwd))
          (t (finalize-inheritance instance)
             (values instance)))))))

(declaim (ftype (function (standard-object)
                          (values &optional standard-object))
                finalize-reachable))



(defgeneric finalize-reachable-superclass (superclass class)
  ;; protocol support for FINALIZE-REACHABLE methods
  (:method ((superclass standard-class)
            (class forward-referenced-class))
    (declare (ignore class))
    (finalize-inheritance superclass)
    (values superclass))

  (:method ((superclass standard-class)
            (class standard-class))
    (declare (ignore class))
    (finalize-inheritance superclass)
    (values superclass))

  (:method ((superclass forward-referenced-class)
            (class forward-referenced-class))
    (class-finalization-note
     "~<Cannot finalize forward-referenced-class ~s~>~
~< as superclass of forward-referenced class ~s~>"
     superclass class))

  (:method ((superclass standard-class)
            (class forward-referenced-class))
    (class-finalization-note
     "~<Cannot finalize standrd-class ~s~>~
~< as superclass of forward-referenced class ~s~>"
     superclass class))

  (:method ((superclass forward-referenced-class)
            (class standard-class))
    (class-finalization-note
     "~<Cannot finalize forward-referenced-class ~s~>~
~< as superclass of ~s~>"
     superclass class)))


(defgeneric finalize-reachable-subclass (subclass class)
  ;; protocol support for FINALIZE-REACHABLE methods
  (:method ((subclass forward-referenced-class)
            (class standard-class))
    (class-finalization-note
     "~<Cannot finalize forward-referenced-class ~s~>~
~< as subclass of ~s~>"
     subclass class))

  (:method ((subclass forward-referenced-class)
            (class forward-referenced-class))
    (class-finalization-note
     "~<Cannot finalize forward-referenced-class ~s~>~
~< as subclass of forward-referenced-class ~s~>"
     subclass class))

  (:method ((subclass standard-class)
            (class forward-referenced-class))
    (class-finalization-note
     "~<Cannot finalize class ~s~>~
~< as subclass of forward-referenced-class ~s~>"
     subclass class))

  (:method ((subclass standard-class)
            (class standard-class))
    (declare (ignore class))
    (finalize-inheritance subclass)
    (values subclass)))


(declaim (ftype (function (class class)
                          (values &optional class))
                finalize-reachable-subclass
                finalize-reachable-superclass))
