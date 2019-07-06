;; iter.lisp - generalized formalisms for typed iteration
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

(defpackage #:ltp/common/iter
  (:use #:ltp/common/mop #:ltp/common #:cl))


;; NB: Towards further formalization in this API design,
;;     furthermore with regards to storage object initialization,
;;     refer to ltp-main:src/main/lsp/stor/README.md


(in-package #:ltp/common/iter)

;; NB: Initial prototype - structure-object encapsulation for iterable storage

(defstruct (iterable
             (:constructor))
  (element-type
   t
   :type type-designator
   :read-only t))


(defstruct (sequence-iterable
             (:include iterable)
             (:constructor))
  (members
   nil
   :type sequence))

;; DNW ....
(defstruct (vector-iterable
             (:include sequence-iterable)
             (:constructor))
  #+FIXME
  (members
   #()
   :type vector))


(defstruct (simple-vector-iterable
             (:include vector-iterable)
             (:constructor %mk-simple-vector-iterable
                           (element-type members)))
  #+FIXME
  (members
   #()
   :type '(simple-array * (*))))


(defun make-simple-vector-iterable (members &key (element-type t))
  (%mk-simple-vector-iterable
   element-type
   (make-array (length members)
               :element-type (upgraded-array-element-type element-type)
               :initial-contents members)))

;; --------------------

;; NB: Initial prototype for "iterable macros"

(defmacro do-simple-vector-iterable ((s whence &optional returnv) &body forms)
  ;; NB: This simple protocol may not provide support for iterable type
  ;; elements to be declared in a manner as to be available to a
  ;; compilation environment -- juxtaposed to any finalized class, in
  ;; any implementation as onto a STANDARD-CLASS
  ;;
  ;; Regardless, there may remain a question as to how provide a type
  ;; declratoin for a static evaluable form, within a macroexpansion,
  ;; for any object that is not available until the macroexpansion is
  ;; evaluated.

  (with-symbols (%s %whence %typ %do-main %returnv %forms)

    ;; NB: ANSI CL is fairly not good at supporting templates for source forms
    ;;
    ;; CL Macro syntax may seem however more advanced than anything
    ;; available in the immediate syntax of CPP macro declarations. The
    ;; call-time evaluation of CL macros provides some particularly
    ;; quirky limitations, however.

    `(let* ((,%whence ,whence)
            ;; TBD: How to  make the binding of %typ available at
            ;; compile time in a lexically scoped environment,
            ;; for any implementation inserting null-lexenv anywhere
            ;; arbitrarily
            ;;
            ;; define-compiler-macro does not in itself provide any
            ;; workaround for the intrinsic quirks/limitations of the
            ;; macroexpansion environment and subsequent breakage for
            ;; some not-evaluated-for-value forms
            (,%typ (iterable-element-type ,%whence)))
       (macrolet ((,%do-main ((,%s ,%whence ,%returnv) ,%forms)
                    (with-symbols (%%whence %memb %len %n)
                      `(let* ((,%%whence ,,%whence)
                              (,%memb (simple-vector-iterable-members ,%%whence))
                              (,%len (length (the simple-vector ,%memb))))
                         (dotimes (,%n ,%len ,,%returnv)
                           (let ((,,%s (svref ,%memb ,%n)))
                             ;; NB: An operation on the class of
                             ;; ,%WHENCE could seem to serve to work
                             ;; around the limitations of the
                             ;; macroexpansion evaluation environment,
                             ;; with the following DECLARE form --
                             ;; except that the binding of ,%WHENCE may
                             ;; be ignored by the compiler, even then,
                             ;; in the macroexpansion.
                             ;;
                             ;; For purpoes of development in this
                             ;; system, it might seem feasible to
                             ;; orchestrate the production of the
                             ;; evaluable form with a compiler
                             ;; macro operating on the iterable
                             ;; object itself, in production of the
                             ;; macroexpansion to the compiler. However --
                             ;; beyond any merely syntactic
                             ;; characteristics of the whole form
                             ;; provided to the compiler macro -- the
                             ;; implementation of such an approach may
                             ;; serve to require an EVAL semantics on
                             ;; the expression denoting the iterable
                             ;; object - quite simply, evaluating it
                             ;; independent of the environment finally
                             ;; created by the compiler, moreover
                             ;; independent of the enviornment in which
                             ;; the form was initially conferred.
                             ;;
                             ;; Subsequently, perhaps the semantics of
                             ;; CMUCL source tranform definitions may
                             ;; seem somehow approachable for
                             ;; consideration, alternate to any
                             ;; limitations as would be imposed from the
                             ;; creation of lexical environments in the
                             ;; compiler itself.
                             ;;
                             ;; As it stands, this Lisp source code DNW:
                             (declare (type ,(symbol-value (quote ,%typ)) ,,s))
                             ,@,%forms
                             ))))))
         (,%do-main (,s ,%whence ,returnv) ,@forms)))
    ))



(eval-when ()

  (setq *break-on-signals* t)

  (let ((iter (make-simple-vector-iterable
               '(1 2 3)
               :element-type #-NIL t #+NIL '(mod 4)))
        (call-reg))
;(macroexpand (quote
    (do-simple-vector-iterable (%elt iter (values call-reg t))
      (npushl (expt %elt 2) call-reg))
;    ))
    )

)

;; --------------------


(defun mk-iterable-macro-function (iterable)
  ;; easy enough to produce, may be non-trivial to bind for "Normal Evaluation"
  (with-symbols (args environment
                      s whence returnv forms)
    (let* ((memb (simple-vector-iterable-members iterable))
           (typ (array-element-type memb)))
    `(lambda (,args ,environment)
       ;; TBD WITH-ENV (PORTABLY)
       ;; For now, although this is probably not actually good - even when
       ;; the compiler produces this behavior after DEFMACRO
       (declare (ignore ,environment))
       (destructuring-bind ((,s &optional ,returnv) &rest ,forms) ,args
         (with-symbols ( %memb %len %n)
           `(let* ((,%memb ,,memb)
                   (,%len ,,(length memb)))
              (dotimes (,%n ,%len ,,returnv)
                (let ((,,s (svref ,%memb ,%n)))
                  (declare (type ,,typ ,,s))
                  ,@,forms
                  )))))))))

#+NIL
(eval-when ()
  (defparameter *v* (make-simple-vector-iterable '(1 2 3)))


  (defparameter *f* (mk-iterable-macro-function  *v*))
  (defparameter *expr*
    '((elt (values call-reg t))
      (npushl (expt %elt 2) call-reg)))


  (funcall (coerce *f* 'function) *expr* nil)

  ;;   (defmacro frob-iter ( ;; HOWTO / TBD

)

;; ------------------------------


(defstruct (list-iterable
             (:include sequence-iterable)
             (:constructor make-list-iterable
                           (%members &key (element-type t)
                                     &aux (members (coerce %members 'list))))
             (:constructor))
  #+FIXME
  (members
   nil
   :type list))


#+TBD
(defmacro do-list-iterable ((s whence &optional returnv) &body forms)
  )

;; ------------------------------------------------------------

;; NB: Initial prototype - standard-object encapsulation for iterable storage

(defclass iterable-class (standard-class)
  ((member-element-type
    :type type-designator
    ;; :access (:read :bind)
    :initarg :member-element-type
    :initform t)))

(defclass iterable-object ()
  ())

(defclass sequence-iterable-object (iterable-object)
  ((members
    ;; FIXME Rename => storage
    :initarg :members
    :type sequence)))

(defclass iterable-vector-class (standard-class)
  ((member-storage-element-type
    :type type-designator
    ;; :access (:read :bind)
    :initarg :member-storage-element-type
    :initform t)))


(defclass vector-iterable-object (sequence-iterable-object)
  ((members
    :type (array * (*)))))

(defclass simple-vector-iterable-object (vector-iterable-object)
  ((members
    ;; :access (:read :bind :unbind)
    ;; ^ NB: see ltp-main:src/main/lsp/base-class/README.md
    :type (simple-array * (*)))))

(defclass list-iterable-object (sequence-iterable-object)
  ((members
    :type list)))



(defgeneric mk-iterable (kind &key &allow-other-keys)
  (:method ((kind vector-iterable-object)
            &key members
              (element-type t) (storage-element-type element-type)
              &allow-other-keys)
    (let* ((len (length members))
           (memb (make-array len
                             :element-kind
                             ;; FIXME: This API needs to be well documented:
                             (storage-element-type (class-of kind))
                             :initial-contents members)))

      ;; NB: This uses any provided MEMBERS sequence as a manner of an
      ;; elements signature, when producing the actual storage vector
      ;; for the iterable
      ))
  )



;; TBD - Storage Protocol & ITERABLE
;;
;; ENSURE-STORAGE-USING-CLASS (CLASS ITERABLE &OPTIONAL INITIAL-CONTENTS)
;; SHARED-INITIALIZE (ITERABLE T &REST INITARGS &KEY INITIAL-CONTENTS &ALLOW-OTHER-KEY
;;
;; [Re]Design LTP-Main ENUM onto ITERABLE-CLASS, ITERABLE-OBJECT
;; adding functions for storage onto symbolic enumeration member objects

;; TBD: HASHED-VECTOR-ITERABLE
;; NB: Onto SXHASH => NUMBER
;;
;; TBD: "Template-like" support for specifying "Where the SXHASH key is stored",
;;      in extensions
;;
;; General usage case: Hash-Table-Like storage for large sets of
;; similarly typed values, indexed onto the SXHASH code computed for one
;; or more fields of each such value.
;;
;; See also: librdf; libxml2 XML Schema Datatypes support; ....

#+ITERABLE-MAP-FUNCTION
(defgeneric ensure-iterable-map-function (return-type iterable)

  ;; TBD: How to effectively parameterize the selection of a per-instance
  ;; static or per-class/instance-dynamic :policy for creation of the iterable
  ;; map function
  ;;    NB: Revise the immediate API => MAP-ITERABLE-STATIC, MAP-ITERABLE-DYNAMIC
  ;;        referring to annotations, subsq.
  ;;    - ENSURE-ITERABLE-STATIC-MAP-FUNCTION
  ;;    - ENSURE-ITERABLE-DYNAMIC-MAP-FUNCTION
  ;;
  ;; Note that any map-function being called exactly once may use a
  ;; static policy - it not differing substantially then, to the
  ;; :dynamic call case, in which some forms generally operating on the
  ;; individual iterable object would not be evaluated until the
  ;; function is called. Conceivably, the :static initialized iterator
  ;; function may serve to provide the compiler with an opportunity to
  ;; further optimize the function, in its compilation.

  (:method ((return-type t) (iterable simple-vector-iterable))

    ;; TBD: Moving the following into a conventional function, e.g
    ;;
    ;; ENSURE-ITERABLE-STATIC-MAP-FUNCTION
    ;; such that may be cached per each instance, under its instance
    ;; values and class state at the time of the function's creation
    ;;
    ;; juxtaposed to a conventional function, e.g
    ;;
    ;; ENSURE-ITERABLE-DYNAMIC-MAP-FUNCTION
    ;; such that may be cached per each class, under that class' state at
    ;; the time of the function's creation
    ;;
    ;; Note that this differentiation may seem somewhat trivial for
    ;; iterable object and iterable class implementations not using
    ;; typed local sequence values for direct storage. Regardless, it
    ;; may be observed that this design may serve to provide an opportunity
    ;; for some optimizations of the compiled forms of functions
    ;; providing support for iterative generalized operations in the
    ;; :static case - while not, per se, preventing applications in the
    ;; :dynamic case
    ;;
    ;; TBD furthermore: Towards concepts of literate programming in a
    ;; system supporting annotation of project resources under an RDF,
    ;; TeX model - e.g using RDF for system reference informatoin, and TeX
    ;; for information pursuant towards presentation in visual
    ;; presentation systems -- if not as an intermediary, direct source
    ;; notation, pursuant to a methodology for deriving XML from
    ;; arbitrary resources annotated with a TeX notation -- moreover for
    ;; audal text-to-speech systems, beyond visual characteristics (per
    ;; se) of graphical resource hyperlinking systems. .... towards
    ;; which, a concept of a model-specific TeX environment may pertain
    ;;
    ;; -- towards generalized usage cases:
    ;;
    ;; - Systems Documentation
    ;; - Systems Review, Analysis, and QA - Support
    ;; - Reusability of Systems - as a singular topic, per se.
    ;;
    ;; ... as may entail some integration with existing source
    ;; documentation systems, e.g gtk-doc, Doxygen, etc.
    ;;
    ;; ... and the presentation environment.
    ;;
    ;; Context: CommonIDE, initially in implementation with generalized
    ;; wigets onto GTK+

    (let* ((result-storage (mk-result-form return-type iterable))
           (form
            (with-symbols (fn)
              `(lambda (,fn)
                 (declare (type (function ,fn)))

                 ;; NB: :STATIC storage for iterable-members, in this
                 ;; prototype - an initial policy, pursuant of some
                 ;; implicit assumptions with regards to applications
                 ;;
                 ;; also, static storage for the ephemeral result object
                 ;; (such that should not be destructively modified by
                 ;; any calling forms, as it may be reused across
                 ;; subsequent calls to this function)
                 ;;
                 ;; Usage Cases e.g
                 ;;
                 ;; - Iterative operations onto static enumerated type
                 ;;   information - note that the caller-provided
                 ;;   function is not hard coded in the function
                 ;;   returned by the following.
                 ;;
                 ;; - Iterative operations onto static model
                 ;;   information - however generally as may subsume
                 ;;   operations onto enumerated type information - e.g
                 ;;   in applications for static model analysis

                 ;; NB: MAP-INTO may seem superfluous if the RESULT-TYPE := NIL
                 ;;
                 ;; The implementation may handle that call-case, implicitly.

                 (map-into ,result-storage
                           ;; ^ NB: Static result value storage (may be NIL)
                           ,(with-frob (elt)
                              ;; NB: Additional wrapping for FN - to
                              ;; provide existing information to the
                              ;; compiler, as for the type of the
                              ;; argument provided to the function FN,
                              ;; assuming that the compiler may then be
                              ;; be to provide additional optimizations
                              ;; onto the call to FN itself.
                              `(lambda (,elt)
                                 (declare (type ,elt
                                                ;; NB: This call would
                                                ;; be the same, across
                                                ;; static and dynamic
                                                ;; policies, as
                                                ;; annotated :
                                                ,(iterable-member-storage-element-type
                                                  (class-of iterable))))
                                 (funcall ,fn ,elt)))
                           ;; NB: Static members storage:
                           ,(iterable-members iterable))
                 (values ,result-storage)))))
      (values (compile* form))

;; TBD - caching for iterable map functions
;;       onto {(CLASS-OF ITERABLE), ITERABLE, RETURN-TYPE}
;;       - Note static storage of iterable members sequences and result
;;         sequences, in the function produced in the example above

;; TBD: MK-RESULT-FORM
;; TBD: MK-RESULT-FORM-USING-CLASS

;; TBD: ENSURE-ITERABLE-MAP-FUNCTION-USING-CLASS

      )))

;; TBD: Beyond the singular specification of :static or :dynamic policy
;; for creation of iterator map functions, this system should endeavor
;; to support a broader facilitation of application of arbitrary system
;; optimization policies, e.g at scales of
;;
;;  {project, {source-section, usage-case+}+}
;;
;; ... for any source section, whether or not implemented in a Common
;; Lisp language.
;;
;; ... presently, regardless of any concept of "Over-Optimization"




(defgeneric map-iterable (fn iterable &key &allow-other-keys)

  ;; TBD API => map-iterable-dynamic (fn iterable)
  #-ITERABLE-MAP-FUNCTION ;; NB: Dynamic/Class-Specific Policy here
  (:method ((fn function) (iterable sequence-iterable-object)
            &key return-type &allow-other-keys)
    ;; NB: FN may assume its own element-type for the iterable
    ;;
    ;; NB: Special eval-case of RETURN-TYPE := NIL
    (map return-type fn (iterable-members iterable)))

  ;; TBD API => map-iterable-static (fn iterable)
  #+ITERABLE-MAP-FUNCTION
  ;; NB: Static/Instance or Dynamic/Class Policy for Creation and
  ;; Storage of the iterable map function as would be used internally,
  ;; within the following
  ;;
  ;; e.g: compute and apply a function singularly for mapping a function
  ;; onto the iterable's effective member objects - the function may be
  ;; compiled and subsequently cached, under any single manner of
  ;; application policy
  ;;
  ;; Note that the mapped function may exit non-locally.
  ;;
  ;; NB: The iterator function must apply the mapped function in a
  ;; reentrant manner.
  ;;
  ;; NB: Under a static policy, the result returned by the following may
  ;; be stored locally to the iterable-map-function, i.e the iterator
  ;; function and - as such - should not be destructively modified.
  ;;
  (:method ((fn function) (iterable iterable-object)
            &key return-type &allow-other-keys)
    (funcall (the function (ensure-iterable-map-function return-type iterable))
             fn))
  )



(validate-class iterable-class)



;; ------------------------------

#+NIL
(defun* map-iterable (fn iterable &optional (return-type t))
  (declare (type function fn)
           (type iterable iterable)
           (values t &optional))
  (let ((memb (etypecase iterable
                (sequence-iterable
                 (sequence-iterable-members iterable))))
        ...
        )))
