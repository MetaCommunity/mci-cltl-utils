
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


(defgeneric finalize-reachable (instance seen-classes)
  ;; NB: An additional method onto this generic function is defined in
  ;;     the SINGLETON system, such that will iterate across direct
  ;;     superclasses of the INSTANCE -- finalizing each direct
  ;;     superclass, when not a forward referenced class, or returning
  ;;     no values from the method. That method will then finalize the
  ;;     INSTANCE itself, and will ensure that any rechable subclass of
  ;;     the INSTANCE is finalized.
  (:method ((instance forward-referenced-class) (seen-classes list))
    (declare (ignore seen-classes))
    (class-finalization-note
     "Cannot finalize foward-referenced class ~S"
     instance))
  (:method ((instance standard-class) (seen-classes list))
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
          ((find instance (the list seen-classes) :test #'eq)
           (values))
          (t (finalize-inheritance instance)
             (values instance)))))))

(declaim (ftype (function (standard-object list)
                          (values t))
                finalize-reachable))


