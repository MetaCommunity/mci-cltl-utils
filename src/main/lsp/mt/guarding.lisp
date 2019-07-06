;; guarding.lisp - WITH-WRITE-GUARD, OBJECT-POOL, GUARDED-FUNCALL - Impl & Suppt
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation onto SB-THREAD
;;
;;------------------------------------------------------------------------------

;; NB: Concerning applications, refer to
;;     ltp-main:src/main/lsp/stor/README.md

;; Ed. NB: The Initial protoype for this implementation has been
;;         developed, as though singularly, onto SBCL SB-THREAD.
;;
;;         Subsequent of further development for DEFPORTABLE.LISP,
;;         this implementation may be generalized for support with other
;;         implementation-specific APIs for program multi-tasking.

;; Ed. NB: When developed for implementation in CCL, the "Write Guard"
;;         concept -- insofar as developed onto GUARDED-FUNCALL --
;;         should be generalized support of Model (R/W) locks, vis a vis
;;         POSIX threads. Such support may be summarized, in a manner,
;;         generally:
;;
;;         - Require mutually exclusive access during write.
;;
;;         - Prevent mutually exclusive access during read, until all
;;           threads holding the nonexclusive read-lock have released
;;           the nonexclusive read-lock
;;
;;         Thus, a READ-LOCK -- in this methodology -- may not be
;;         updated to a WRITE-LOCK until the thread requesting to
;;         update to a WRITE-LOCK will be the first requesting thread
;;         for a WRITE-LOCK onto the same modal (R/W) lock
;;
;;         See also: The FreeBSD kernel, which provides a number of
;;         forms for facilitating concurrent access to kernel resources
;;         - moreover, as extensively described in formal documentation
;;         about the FreeBSD kernel.


(in-package #:ltp/common/mop)
;; TBD: mk local defpackge - reusing LTP/COMMON/MOP only temporarily, here

(declaim (type #+(and SBCL SB-THREAD) sb-thread:mutex
               #+(and SBCL (not SB-THREAD)) null
               #-SBCL nil ;; NB: Consider this onto DEFPORTABLE.LISP

               ;; NB: In effect, a type declaration for a symbol as
               ;; having the type NIL, in any single lexical
               ;; environment, may serve to prevent any binding of a
               ;; value onto that symbol, in the same lexical
               ;; environment.

               +write-guard-1+))

(defconstant* +write-guard-1+
    ;; NB: see also ./defportable.lisp

    ;; NB: In the initial prototype -- using mutually-exclusive locks in
    ;;     implementation, vis a vis SB-THREAD -- This +WRITE-GUARD-1+ is
    ;;     to be held only for the duration of checking, and optionally
    ;;     initializing, and subsequently releasing a lock for any
    ;;     guarded location - such as of the MEMBERS slot of an
    ;;     OBJECT-POOL, or the guarded FUNCTION of a GUARDED-FUNCALL
    ;;
    #+(and SBCL SB-THREAD)
    (sb-thread:make-mutex :name "+write-guard-1+")
    #+(and SBCL (not SB-THREAD))
    nil
    #-SBCL
    (error "+write-guard-1+ not supported in this implementation")
    "Generalized initial write-guard for instance-localized, mutually
exclusive access locking protocols"
    )


;; NB: SBCL SB-SYS:SIGNAL-DEADLINE
;;
;;    - used in SB-THREAD::%WAIT-FOR-MUTEX ... which is used in
;;      SB-THREAD:GRAB-MUTEX when a MUTEX cannot be initially acquired
;;
;;    - signals an error of type SB-SYS:DEADLINE-TIMEOUT within a
;;      lexical environment in which the restarts DEFER-DEADLINE and
;;      CANCEL-DEADLINE are avaialble - the former, such that the
;;      restart's main function would reinitialize the deadline and
;;      (in effect) defer to wait for a number of additional
;;      <seconds>. That number of additional <seconds> may be specified
;;      via TRY-RESTART, or the convenience function DEFER-DEADLINE.
;;      By default, the same number of <seconds> will be used as was
;;      specified to the previous deadline-wait entry. The restart
;;      CANCEL-DEADLINE may be called directly as to "Gracefully exit"
;;      from the location in which the initial deadline-wait was
;;      initialized. The dedline itself is implemented with the
;;      structure class SB-IMPL::DEADLINE. See also: SB-THREAD:GRAB-MUTEX
;;
;;    - may serve to provides a basis for a calling application to
;;      handle the circumstance of when a blocking lock request has
;;      failed due to timeout
;;
;;    - does not in itself serve to provide a basis for a calling
;;      application to handle the circumstance of when a non-blocking
;;      lock request has failed due to mutual exclusion


(defmacro with-write-guard ((which &key block-p timeout
                                   when-timeout-fail
                                   when-acquire-fail)
                            &body body)
  ;; NB: Not guaranteed to be portable "Out of the box"

  ;; TBD: This macro's definition may be moved into a source system
  ;;      independent of GUARDED-FUNCALL, as it may be of some general
  ;;      usage in applications without GUARDED-FUNCALL, per se.

  ;; NB: For SBCL w/ SB-THREAD the WHEN-FAIL form may be called only
  ;;     when a TIMEOUT is specified and exceeded
  ;;
  ;; NB: In this prototype, the WHEN-ACQUIRE-FAIL form should be called
  ;;     for any non-blocking request that fails to acquire the lock
  ;;     denoted in WHICH. This has been approached, below, using a
  ;;     simple methodology in application of two lexically scoped
  ;;     varaibles -- one, storing a lexically available symbol,
  ;;     the other initially storing the value NIL. It may be assumed
  ;;     that the value of the let-var storing the value NIL will be set
  ;;     to be EQ to the value of the let-var storing the original
  ;;     symbol only when the lock acquisition has succeeded.
  ;;
  ;;     This methodology may be portable onto other implementations.
  ;;
  ;;     TBD: Portability for the implemeentation onto SBCL DEADLIMNE


  ;; TBD: Organizing this source system in a manner as to ensure that
  ;; each set of implementation-specific forms will be defined in a
  ;; single source file for the specific implementation.
  ;;
  ;; Alternately, see also: DEFPORTABLE.LISP

  #+(and SBCL SB-THREAD)
  (with-symbols (%which %block-p %timeout %time-fail %acq-fail %c
                        %thunk %thunk-p)
    `(let ((,%which ,which)
           (,%block-p ,block-p)
           (,%timeout ,timeout)
           (,%time-fail ,when-timeout-fail)
           (,%acq-fail ,when-acquire-fail))
       (declare (type (or null function) ,%time-fail ,%acq-fail))

       ;; FIXME: Only emit the following warnings in the macroexpansion,
       ;; under certain policy configurations - it can be non-portable,
       ;; here,  as this branch is already evaluated locally to SBCL
       (when (and ,%time-fail (null ,%timeout))
         (simple-style-warning "~<WITH-WRITE-GUARD called with ~>~
~< non-null WHEN-TIMEOUT-FAIL but null TIMEOUT~>"))
       (when (and ,%timeout (null ,%block-p))
         (simple-style-warning "~<Intrinsic blocking call to ~
WITH-WRITE-GUARD~>~< assuming TIMEOUT ~S overriding null BLOCK-P~>" ,timeout))

       (let ((,%thunk (make-symbol "%thunk-internal"))
             (,%thunk-p nil))
         (declare (type symbol ,%thunk ,%thunk-p)
                  (dynamic-extent ,%thunk ,%thunk-p))
         (handler-bind ((sb-sys:deadline-timeout (,%c)
                          (cond
                            ((functionp ,%time-fail)
                             (funcall (the function ,%time-fail) ,%which ,%c))
                            (t (sb-sys:cancel-deadline ,%c)))))
           (unwind-protect
                (sb-thread:with-mutex (,%which
                                       :waitp (or ,%block-p ,%timeout)
                                       :timeout ,%timeout)
                  (unwind-protect
                       (progn ,@body)
                    (setq ,%thunk-p %thunk)))
             ;; NB: Try to provide a WHEN-ACQUIRE-FAIL support for
             ;; non-blocking calls
             (unless (eq ,%thunk-p ,%thunk)
               (when ,%acq-fail
                 (funcall (the function ,%acq-fail) ,%which))))))))
  #+(and SBCL (not SB-THREAD))
  `(progn ,@body) ;; TBD: Ever single-threaded implementation case
  #-SBCL ;; TBD: Every unsupported multi-threaded implementation case
  (error "WITH-WRITE-GUARD not yet supported in this implementation")
  )


;; --

;; TBD - OBJECT-POOL should be generally portable
;;
;; Note, however ... the pool-members lock
;;  and thus also, +WRITE-GUARD-1+
;; for any multi-threaded implementation.
;;
;; Note also, MAKE-HASH-TABLE :synchronized t ; in some SBCL


;; Concept - Effective MUTEX pooling for GUARDED-FUNCALL, in supported impls
;;
;; See also: +WRITE-GUARD-1+

(deftype sxhash-key ()
  ;; NB: This assumes that no CONDITION object will be tested for
  ;; consistency onto the SXHASH-KEY type - otherwise could be defined as:
  ;;   (and (not instance) (not condition))
  ;;
  ;; See also: QA for the INSTANCE deftype in LTP/COMMON/MOP
  '(not instance))


#+TBD
(declaim (ftype (function (sxhash-key sxhash-key)
                          (values boolean &optional))
                sxhash-key-eql))
#+TBD
(defun sxhash-key-eql (obj1 obj2)
  (= (sxhash obj1) (sxhash obj2)))


#+TBD
(defstruct
    (object-pool
      #+TBD
      (:constructor %make-object-pool (... %objects instance-guard)))
  ;; NB: Concurrent access to an OBJECT-PO0L must be, in itself,
  ;; guarded.
  ;;
  ;; This implementation will reuse the same pattern extended for
  ;; GUARDED-FUNCALL, as of using a +WRITE-GUARD-1+ pursuant
  ;; to initialization, checking, or <<free+deallocation>> of an
  ;; instance-local write lock
  (%objects
   (make-array 16)
   :type simple-vector
   ;; TBD @ IMPL/QA: The slot's binding should be understood as
   ;; read-only - more or less, static/constant as a slot value
   ;; binding. However, the object stored in the slot value, itself,
   ;; will be destructively modified.
   :read-only t)
  #+TBD
  (object-allocation-table
   ;; NB/Concept: During RELEASE-OBJECT, with the POOL-WRITE-GUARD held,
   ;; scan this simple bit vector for the first index having a value 0.
   ;; Use that index as an index onto the %OBJECTS simple-vector.
   ;;
   ;; If no index can be found with value 0, the pool is "Full" and the
   ;; object can be discarded
   ;;
   ;; Note that this would be applicable for any type of object in the
   ;; %OBJECTS table. It may be redundant, however, for many types of
   ;; OBJECT in active storage - as where a simple "non-nil" test onto
   ;; the %OBJECTS simple vector may be sufficient.
   (make-array 16 :element-type 'bit)
   :type simple-bit-vector
   :read-only t)
  (depth
   16 ;; NB arbitrary initform
   :type array-length
   :read-only t)

  (last-free-index
   ;; NB: Caching for storage onto the members-pool, may be useful in
   ;; any generalized task in which ALLOCATE-OBJECT is called on an
   ;; OBJECT-POOL, followed immediately with  RELEASE-OBJECT onto the
   ;; same OBJECT-POOL - thus avoiding iteration onto the %OBJECTS
   ;; vector, in the call to any RELEASE-OBJECT for that OBJECT-POOL
   0
   :type (or null array-index)
   :read-only nil)

  (name
   (make-symbol "Anonymous Object Pool")
   ;; NB: using the SXHASH-KEY type in an interest of support for
   ;; portable object-pool naming onto arbitrary application-specific
   ;; naming schema
   :type sxhash-key
   :read-only t)

  ;; TBD: Porting this pattern onto C callbacks @ GTK+

  (initialize-new-function
   ;; ...
   :type function
   :read-only t
   )
  (initialize-exists-function
   ;; ...
   :type function
   :read-only t
   )
  (deinitialize-exists-function
   ;; ...
   :type function
   :read-only t
   )

  (pool-write-guard
   ;; ...
   (impl-ecase
    ((and :SBCL :SB-THREAD) (error "Uninitialized..."))
    (:SBCL nil))
   :type (impl-ecase
          ;; FIXME: This may not parse well, during DEFSTRUCT eval.
          ;;
          ;; Consider DEFTYPE WRITE-GUARD w/ IMPL-ECASE
          ;;
          ;; TBD: "PORTING THIS" onto the modal (R/W) locks API avl in CCL
          ((and :SBCL :SB-THREAD) SB-THREAD:MUTEX)
          (:SBCL null))
   :read-only t)

  )


(defconstant* +object-meta-pool+
    (make-hash-table :test #'equal
                     #+SBCL :synchronized #+SBCL t))


(defmacro define-ftype (args-type values-type &rest which-fns)
  `(declaim (ftype (function ,args-type ,values-type)
                   ,@which-fns)))

(define-ftype (sxhash-key &optional t)
    (values (or object-pool null) &optional)
  find-object-pool)


(defun find-object-pool (name &optional errorp)
  (multiple-value-bind (pool fp)
      (gethash name +object-meta-pool+)
    (declare (type boolean fp))
    (cond
      (fp (values pool))
      (errorp (error "No object pool registered for name ~S" name))
      (t (values nil)))))

;; NB This assumes that no scheduling concern may occur between
;;    generalized tasks of (A) object-pool initialization and
;;    (B) subsequent access for retrieving any single named
;;    object pool onto +OBJECT-META-POOL+

#+TBD
(declaim (ftype (function (...)
                          (values object-pool boolean &optional))
                ensure-object-pool))

#+TBD
(defun ensure-object-pool (name ..args check-initialized-for-args-p)
  ;; NAME := SXHASH-KEY
  (let ((pool (find-object-pool name nil)))
    (cond
      (pool (values pool) nil)
      (t
       (setq pool (frob-new-pool name ..args))
       (setf (gethash name +object-meta-pool+)
             pool)
       (values pool t)))))


#+TBD
(defun allocate-object (initarg pool))
;; ^ INITARG := single value for the object pool's INITIALIZE-NEW-FUNCTION
;;              or first arg for the object pool's INITIALIZE-EXISTS-FUNCTION
;; ^ may return an object previously stored in the pool members vector,
;;   or a newly created object, when the pool is empty

#+TBD
(defun release-object (object pool))
;; ^ If pool is full, discard OBJECT and release the pool members lock, otherwise store OBJECT.
;;   If stored, call the pool's deinitialize-exists-function on the object
;;   then release the pool members lock



;; SUBSQ: Create an OBJECT-POOL of implementation-specific mutex/... objects
;;        for usage in GUARDED-FUNCALL. NB: Ensure that this singular
;;        object pool will be accessed by direct reference, not via its name


;; --

(defstruct (guarded-funcall
             (:constructor %mk-guarded-funcall (function)))
  (function
   (load-time-value #'(lambda () (error "Uninitialized GUARDED-FUNCALL-FUNCTION"))
                    t)
   :type function
   :read-only t)
  #+(and SBCL SB-THREAD)
  ;; NB: SBCL -- as yet -- does not provide nay intrinsic support for
  ;;     modal (read/write) locking, vis a vis the POSIX threads API, in
  ;;     any build. Considering some concerns as may attend with
  ;;     arbitrary patches onto arbitrary calling conventions within
  ;;     arbitrary operating systems on arbitrary hardware/machine
  ;;     architectures, rather than trying to port any methodology for
  ;;     modal read/write locking onto SBCL, this implementation-specific
  ;;     code will use exclusive (write) locks for every access to a
  ;;     guarded form, in SBCL. As such, it may be assumed to not be as
  ;;     theoretically responsive for applications in which any
  ;;     significant number of non-exclusive "read" locks would be
  ;;     useful -- assuming any significant number of concurrent
  ;;     accesses to individual objects held under any exclusive
  ;;     GUARDED-FUNCALL-WRITE-GUARD.
  ;;
  ;; NB: While a GUARDED-FUNCALL-WRITE-GUARD is being checked, initialized,
  ;;     or freed, the singular +WRITE-GUARD-1+ must be held
  ;;     within the calling thread. This should serve to ensure safe
  ;;     conccurrent access to individual GUARDED-FUNCALL-WRITE-LOCk
  ;;     objects, and thus should serve to allow for safely initializing
  ;;     a GUARDED-FUNCALL-WRITE-LOCK from the GUARDED-FUNCALL-WRITE-GUARD-POOL[TBD],
  ;;     pursuant of making a dynamically guarded funcall to the
  ;;     GUARDED-FUNCALL FUNCTION, as guarded with an allocated
  ;;     WRITE-GUARD,  -- "In Theory".
  ;;
  ;;     .. if it may not serve to make the GUARDED-FUNCALL WRITE-GUARD, in
  ;;     effect, redundant.
  ;;
  ;;     [Indemnification Clause Here]
                                        ;
  (write-guard
   ;; TBD: Whether this may be implemented as a semaphore in the SBCL
   ;; SB-THREAD API, together with +WRITE-GUARD-1+
   nil
   :type (or null sb-thread:mutex )
   :read-only nil
   ))


;; NB: subsq of development of DEFPORTABLE.LISP,
;;     continue implementing GUARDED-FUNCALL
