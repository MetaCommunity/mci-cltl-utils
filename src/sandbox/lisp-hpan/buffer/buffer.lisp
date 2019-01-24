;; an early prototype for a generic buffering library
;;
;; abandoned due to generic complexity, 25 Dec 2015


(in-package #:cl-user)


(eval-when (:compile-toplevel :load-toplevel :exeucte)

  #+asdf
  (asdf:operate 'asdf:load-op '#:bordeaux-threads)

  (defpackage #:buffer
    (:use #:libfoo #:bt #:cl)
    (:export ) ;; ...
    )
  
  )

(in-package #:buffer)

(defgeneric buffer-contents (buffer))
(defgeneric (setf buffer-contents) (new-contents buffer))

(defgeneric buffer-element-type (buffer))

(defgeneric buffer-test-function (buffer))
(defgeneric (setf buffer-test-function) (new-test-function buffer))

(defgeneric buffer-lock (buffer))


(defclass buffer ()
  ;; encapsulation of:
  ;; 1) a buffer contents object, e.g. an adjustable array
  ;; 2) a test function, encapsulating a key function such as for FIND-IF
  ((contents
    :initarg :contents
    :accessor buffer-contents)
   (element-type
    :initform t
    :initarg :element-type
    ;; :type libfoo:type-designator
    :reader buffer-element-type)
   (test-function
    :initarg :test-function
    :type function
    :accessor buffer-test-function)
   (lock
    :initarg :lock
    ;; :type lock ;; TO DO : Portable 'lock' type in Bordeaux Threadds (PATCH)
    :reader buffer-lock
    ;; TO DO: Modal locks for BT
    )))

(defun slot-initialize-p (name instance slots) ;; UTIL
  ;; Utility for conditional slot value initialization
  (declare (type symbol name)
           (type (or cons boolean) slots)
           (values boolean &optional))
  (and (cond
         ((consp slots)
          (and (member name (the cons slots) :test #'eq)
               (values t)))
         ((eq slots t)
          (values t))
         (t (values nil)))
       ;; check that the slot has not been already bound to a value
       (not (slot-boundp name instance))))


;; (slot-initialize-p 'print-object ... t)

;; (slot-initialize-p 'print-object ... '(print-object format))

;; (slot-initialize-p 'print-object ... '(format))


(defmethod shared-initialize :around ((instance buffer) slots
                                      &rest initargs
                                      &key test-function
                                        lock
                                        &allow-other-keys)
  (let (args-updated-p)
    
    (when (and test-function
               (not (functionp test-function)))
      ;; ensure test-function is a function
      (let ((f (coerce test-function 'function)))
        (setf (getf initargs :test-function) f)
        (setq args-udpated-p t)))

    (when (slot-initialize-p 'lock instance slots)
      (let ((lock (make-lock (format nil "buffer-lock ~A" instance))))
        (setf (getf initargs :lock) lock)
        (setq args-updated-p t)))
    
    (when (next-method-p)
      (cond
        (args-updated-p
         (apply #'call-next-method instance slots initargs))
        (t (call-next-method))))))
    

(defmacro with-locked-buffer ((buffer
                               #+modal-locks &optional
                               #+modal-locks mode)
                              &body body)
  (let ((%lock (gensym "%lock-")))
    `(let ((,%lock (buffer-lock ,buffer)))
       (with-lock-held (,%lock
                        #+modal-locks :mode
                        #+modal-locks ,mode)
         ,@body
         ))))



(defgeneric make-buffer-find-function (buffer))


(defgeneric buffer-find (key buffer &optional errorp)
  (:method (key (buffer buffer) &optional (errorp t))
    (with-locked-buffer (buffer #+modal-locks :mode #+modal-locks :read)
      (funcall (buffer-find-function buffer) key errorp)
      )))

(defgeneric make-buffer-push-function (buffer))

(defgeneric buffer-push (object buffer)
  (:method (object (buffer buffer))
    (with-locked-buffer (buffer #+modal-locks :mode #+modal-locks :write)
      (funcall (buffer-push-function buffer) object)
      )))


(defgeneric make-buffer-remove-function (buffer))

(defgeneric buffer-remove (key buffer)
  (:method (key (buffer buffer))
    (with-locked-buffer (buffer #+modal-locks :mode #+modal-locks :write)
      (funcall (buffer-remove-function buffer) key)
      )))


(defclass sequence-buffer (buffer)
  ((contents
    :type sequence)))


(defgeneric buffer-unbound-marker (buffer))

(defclass vector-buffer (sequence-buffer)
  ;; Prototype : advise.lisp
  ((contents
    :type vector)
   (unbound-marker
    :initarg :unbound-marker
    :reader buffer-unbound-marker)))


;; (declare (type array-dimension-designator %buffer-expand%)) ;; FIXME : IMPORT

(defvar %buffer-expand% 8)

(defmethod shared-initialize :around ((instance vector-buffer)
                                      slots &rest initargs
                                      &key (unbound-marker nil unbound-marker-p)
                                        &allow-other-keys)
  (declare (ignore unbound-marker))

  (let (args-updated-p)
    
    (when (and (not unbound-marker-p)
               (slot-initialize-p 'unbound-marker instance slots))
      (setf (getf initargs :unbound-marker)
            (allocate-instance (class-of instance)))
      (setq args-updated-p t))

    (when (next-method-p)
      (cond
        (args-updated-p (apply #'call-next-method instance slots initargs))
        (call-next-method)))
  
    (when (slot-initialize-p 'contents instance slots)
      (let ((v (make-array %buffer-expand%
                           :element-type (buffer-element-type instance)
                           :fill-pointer 0
                           :initial-element (buffer-unbound-marker instance)
                           :adjustable t)))
        (setf (buffer-contents instance) v)))))
  


;; (defmethod make-buffer-find-function ((buffer vector-buffer))
;;   (let ((test (buffer-test-function buffer))
;;         (unbound (buffer-unbound-marker buffer)))
;;     `(lambda (object &optional (errorp t))
;;        (or (find-if ,test (the vector (buffer-contents buffer))
;;                     ))))


(defmethod make-buffer-remove-function ((buffer vector-buffer))
  (let ((test (buffer-test-function buffer))
        (not-found (gensym "%not-found-")))
    `(lambda (key &optional (errorp t))
       (delete-if ,test (the vector (buffer-contents buffer))))))


