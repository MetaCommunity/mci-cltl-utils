;; acc-gen.lisp - Accessor Function Definition for ltp-base-class-fdef system
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:ltp/base-class/accessor)

#+NIL
(deftype subtype (name)
  (labels ((locally-check-one-subtype (spec)
             (subtypep spec name)))
    ;; DNW - the type expansion may not be storing the lexical
    ;; environment in which it was defined
    `(satisfies locally-check-one-subtype)))

;; (typep '(find-class 'simple-base-string) '(subtype simple-string))

#+TBD
(deftype finalized-class (&optional (name t))
  ;; The ,NAME spec below DNW except for the class named T.
  ;;
  ;; In order to provide a (SATISTFIES SUBTYPE-OF-<TYPE>) predicate
  ;; for an arbitrary NAME, this DEFTYPE could use a methodology for
  ;; defininig arbitrary functions for purposes of subtype eval, e.g
  ;; SUBTYPE-OF-STRING.
  ;;
  ;; However, such a methodology could be thought to comprise a manner
  ;; of a memory leak.
  `(and class ,name (satisfies class-finalized-p)))

;; (typep (find-class 'string) 'finalized-class)
;; => T

;; (typep (find-class 'simple-base-string) '(finalized-class simple-string))
;; ^ DNW

(deftype finalized-class ()
  `(and class (satisfies class-finalized-p)))

(define-condition unfinalized-class (type-error)
  ;; FIXME - move into a base-class shared system; reuse in LTP singleton
  ()
  (:default-initargs :expected-type 'finalized-class)
  (:report
   (lambda (c s)
     (format s "~<Not a finalized class:~>~< ~S~>"
             (type-error-datum c)))))


;; (declaim (inline ensure-class-finalized-p))

(defun* ensure-class-finalized-p (class)
  ;; FIXME: Move this source definition and the corresponding type and
  ;; condition class definitions, above, into ltp/common/mop src
  ;;
  ;; NB: See also, alternate definition with annotations
  ;; ltp-main:src/test/lsp/with-condition-restart.lisp
  ;;
  (declare (type class-designator class)
           (values class &optional))
  (let ((%class (compute-class class)))
    (declare (type class %class))
    (restart-case
        (cond
          ((class-finalized-p %class)
           (values %class))
          (t
           (error 'unfinalized-class
                  :expecteted-type 'finalized-class
                  :datum %class)))
      (finalize ()
        ;; ASSUMPTION: That this restart will be invoked only as a
        ;; result of the failed CLASS-FINALIZED-P condition, denoted
        ;; above, rather than by any form resulting from CLASS-FINALIZED-P
        :report "Finalize Class"
        (finalize-inheritance %class)
        (values %class)))))

;; (ensure-class-finalized-p 'string)

;; (class-finalized-p (ensure-class-finalized-p (defclass #.(make-symbol "frob") () ())))
;; should -> T once the FINALIZE restart is invoked

;; NB: An extension onto STANDARD-CLASS may be defined as to ensure that
;; any class is finalized before it may be subsequently used, pursuant
;; of a class' defintion as other than a forward-referenced class.
;;
;; Some extensions onto STANDARD-CLASS moreover may be defined as to
;; require that a class will not be redefined once it is finalized.
;;
;; In such extensions, ENSURE-CLASS-FINALIZED may be -- in effect --
;; redundant.
;;
;; Regardless, perhaps the following source forms may be applied about
;; non-finalized classes - an error condition, such that this program
;; will endeavor to signal as such.

;; ----------

#|

  # Remarks - Limitations on Design

  ## Class Layouts and Slot Value Storage in PCL w/ Multiple Inheritance in CLOS

  - Juxtapose to class layouts and slot value storage in pcl w/ single
    inheritance onto CL:DEFSTRUCT

  - Note also, multiple inheritance in CL:DEFINE-CONDITION

  * In a practical regard, this may serve to denote a limitation
    affecting definition of generally defstruct-like accessors onto
    CLOS, if actuated singularly about direct slot definitions
    extensionally in MOP.

  * Theoretically, a program should not assume that the efective slot
    definition S_E_1 -- by way of slot definition name -- corresponding
    to a direct slot definition S_D_1 in a class C_1 will have the same
    effective _layout location_ as an effective slot definition S_E_2
    representing the slot described by S_D_1 but in a class C_2, for C_2
    defined as a subclass of C_1.

    To address this concern, In a practical regard, any accessor defined
    after S_D_1, when applied to an instance of any class not eq to C_1,
    may dispatch as to make an "Ordinary" call to the MOP function.
    SLOT-VALUE-USING-CLASS (or any analogous metaobject accessor form,
    e.g SETF form or SLOT-BOUNDP metaobject accessor form). While
    perhaps convenient from a perspective of program design, this
    approach may be believed to be non-optimal for access onto any slot
    of any subclass of C_1. As such, the accessor may instead produce an
    error of a subtype of TYPE-ERROR when provided an instance of any
    class not EQ to the class C_1. (Note that this, in itself, does not
    serve to provide any limitations towards accessor calls onto
    dynamically redefined classes, while it may serve to address
    limitations with regards to class slot layout in subclasses.)

  * In order to address -- by way of an API limitation -- concerns for
    slot location accessors, as entailed afyer dynamic redefinition of a
    class, this source system may subsequently be defined with
    limitation for applications onto BASE-CLASS, such that a BASE-CLASS
    may be defined as to not permit class redefinition subsequent of
    class finalization.

      * This may be accomplished, in some regards, with a trivial error
        procedure in a method REINITIALIZE-INSTANCE (BASE-CLASS)

      * NB: This may entail some updates to the LTP definition of the
        class SINGLETON, pursuant of refactoring the definition of the
        classes SINGLETON and PROTOTYPE-CLASS onto BASE-CLASS. In a
        manner, moreover the LTP SINGLETON class definition may serve to
        provide an initial usage case for QA about the design and
        implementation of the slot location accessor subset of
        functionality, pursuant towards generalized support for a
        "DEFCLASS like DEFSTRUCT" semantics in definition of LTP
        BASE-CLASS class metaobjects. In a practical regard, forms using
        the functions LTP/COMMON/MOP:FINALIZE-REACHABLE -- as within
        CHANGE-CLASS (FORWARD-REFERENCED-CLASS PROTOTYPE-CLASS) and in
        SHARED-INITIAIZE :AFTER (SINGLETON T) -- may be considered for
        subsequent update, after initial testing with regards to the
        limitation of preventing redefinition of a finalized class for
        any BASE-CLASS.

        The definition of ALLOCATE-INSTANCE (SINGLETON) may also be
        reconsidered, during such refactoring of the design of
        LTP/COMMON/SINGLETON:SINGLETON and
        LTP/COMMON/SINGLETON::PROTOTYPE-CLASS classes - both defined
        under the LTP source system, `ltp-common-singleton`

|#

;; ----
;;
;; General prototypes (FIXME - Redundant)
;;

#+TBD
(defgeneric compute-accessor-ftype (slotdef class))

#+TBD
(defgeneric compute-accessor-lambda (slotdef class))
;; ^ TBD Interop. w/ LAMBDA*, COMPUTE-ACCESSOR-FTYPE
;; --> COMPUTE-ACCESSOR-DEFUM-EXPANSION

#+TBD
(defun* compute-direct-accessor-ftypes (class)
  (declare (type class class)
           (values list &optional))

  (%ensure-class-finalized-p class)
  )


;; ----
;;
;; Some implementation-specific functions
;;

#+(and SBCL TBD)
(defun instance-slot-location (slotd)
  ;; FIXME-TBD - cf. STANDARD-INSTANCE-ACCESS as implemented in PCL and
  ;; subsq. in SBCL
  )

#+SBCL
(defun instance-slot-boundp (slotd instance)

  ;; utility form, may be applied in ASSERT-SLOT-BOUNDP

  ;; NB: This prototypr has not, as yet, been accompanied with any
  ;; comprehensive review of how SLOT-INFO functional members are
  ;; initialized in a "Built" SBCL, or during SBCL cross-compile.

  ;; ASSUMPTION: INSTANCE is not a FUNCALLABLE-STANDARD-INSTANCE [FIXME]
  ;;
  ;; See also: SBCL SLOT-BOUNDP defun
  ;;
  ;; ... although, in this function's definition - which differs to SBCL
  ;; SLOT-BOUNDP in that this function allways calls the slot-info
  ;; boundp function ... perhaps this form may be applicable for
  ;; funcallable instances, if not also for conditions and structure
  ;; objects, in SBCL [FIXME NEEDS TEST]

  ;; ASSUMPTION: SLOTD represents a slot definition valid for the class
  ;; of INSTANCE, throughout the duration of this function's evaluation

  ;; NB: This was referenced onto the source code of SBCL SLOT-BOUNDP,
  ;;     furthermore referenced onto the object description of the
  ;;     SB-PCL WRAPPER object as such - denoted as a a class signature,
  ;;     below.
  ;;
  ;; This may be fairly portable to CMUCL.
  (let* ((sl-name (slot-definition-name slotd))
         (signature (sb-pcl::valid-wrapper-of instance))
         (sl-info (sb-pcl::find-slot-cell signature sl-name)))
    ;; FIXME: This does not err if SL-INFO is null, as it assumes SLOTD
    ;; will represent a slot defined in the instance's class, throughout
    ;; the duration of this function's evaluation.
    (funcall (sb-pcl::slot-info-boundp (cdr sl-info))
             instance)))

#+(and SBCL NIL)
(eval-when  ()

  (defclass frob ()
    ((sl-a
      :initarg :sl-a)
     (sl-b
      :allocation :class
      :initarg :sl-b)))

  (finalize-reachable (find-class 'frob) nil)

  (sb-pcl::find-slot-cell (sb-pcl::valid-wrapper-of (find-class 'frob))
                          'sl-a)
  ;; NIL ??


  (position 'sl-a (sb-pcl::layout-slot-list
                   (sb-pcl::valid-wrapper-of (find-class 'frob)))
            :key #'slot-definition-name :test #'eq)
  ;; NIL

  (describe (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob))))


  ;; NB: CLASS-PROTOTYPE is certainly not the most effective way to find
  ;; the class' wrapper

  (sb-pcl::find-slot-cell
   (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob)))
   'sl-a)
  ;; => <CONS> with FIXNUM CAR

  (sb-pcl::find-slot-cell
   (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob)))
   'sl-b)
  ;; => <CONS> with NIL CAR (!) / NB: Class allocated slot

  (funcall (sb-pcl::slot-info-boundp
            (cdr
             (sb-pcl::find-slot-cell
              (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob)))
              'sl-b)))
           (class-prototype (find-class 'frob)))
  ;; => NIL


  (funcall (sb-pcl::slot-info-boundp
            (cdr
             (sb-pcl::find-slot-cell
              (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob)))
              'sl-a)))
           (class-prototype (find-class 'frob)))
  ;; => NIL


  (describe
   (sb-pcl::slot-info-boundp
    (cdr
     (sb-pcl::find-slot-cell
      (sb-pcl::valid-wrapper-of (class-prototype (find-class 'frob)))
      'sl-b))))


  ;; --

  (instance-slot-boundp (find 'sl-a (class-slots (find-class 'frob))
                              :test #'eq :key #'slot-definition-name)
                        (class-prototype (find-class 'frob)))
  ;; => NIL


  ;; test for a class-alloated slot
  (instance-slot-boundp (find 'sl-b (class-slots (find-class 'frob))
                              :test #'eq :key #'slot-definition-name)
                        (class-prototype (find-class 'frob)))
  ;; => FIXME QUIRKS & NONSENSE

  (defparameter *frob-1* (make-instance 'frob
                                        :sl-a 1 :sl-b 2))

  (instance-slot-boundp (find 'sl-a (class-slots (find-class 'frob))
                              :test #'eq :key #'slot-definition-name)
                        *frob-1*)
  ;; => T

  (instance-slot-boundp (find 'sl-b (class-slots (find-class 'frob))
                              :test #'eq :key #'slot-definition-name)
                        *frob-1*)
  ;; => T


)

;; ----
;;
;; Generalized unparser functions
;;
;; NB/Design Documentation: An approach producing "Intrinsic Defun"
;; calls within DEFMACRO may seem preferrable, in some regards, to the
;; generalized  "Print Source" approach developed here.
;;
;; This "Print Source" approach -- in effect -- serves to require that
;; at least one of an epehmeral string output stream or effective system
;; source file ;would be created, pursuant to compiling the printed
;; source forms, during any single "Build" in which this system may be
;; used. Of course, the generated source forms may also be directly
;; reviewed, in a manner generally analogous to reviewing the output of
;; MACROEXPAND or MACROEXPAND-1 for a normative macro function call.
;;
;; It might be assumed that if a string output stream is used, any
;; implementation bytecode -- i.e "FASL" information -- such as would be
;; produced during the compilation of the printed source forms, will
;; have been written to a file specified, through any methodology,
;; within the calling lexical environment. Thus, assuming that the
;; source forms have been generated in a manner suitable for any
;; singular manner of systems specification, the FASL forms produced
;; during the compilation could be reused during any later runtime
;; sessions, even if the generated source forms -- e.g if having been
;; written temporarily to any one or more of an ephemeral string output
;; stream -- may not be in themselves immediately available, at that
;; time.
;;
;; This prototype may be accompanied with an extension onto ASDF, for
;; facilitating normal application of this system in extension to any
;; single class definition. Of course, anyone applying this system as
;; such should be assumed to have assumed all responsibilities
;; contingent of distribution of software bytecode and software source
;; code -- pursuant of any formal acceptance of licensing terms, for any
;; software that may be, in effect, extended with such generated source
;; code. In any formal regards, the indemnification clause put forward
;; in this system's licensing terms should be assumed to apply.
;;
;; That being addressed, in hoewver, this system may endeavor to provide
;; some utility forms such that may serve to extend ASDF for source
;; generation in applications of this system. Ideally, such extension
;; should serve to provide some manner of a facility for support of
;; software systems management -- not limited to any matters as may be
;; contingent specifically of evaluatoin of Common Lisp source code, e.g
;; the specification of an IN-PACKAGE form in an output stream, previous
;; to any call to PRINT-READER-FOR onto that output stream. This
;; initiative, itself, may be furthermore continued under the CommonIDE
;; project.


;; TBD: Applciation of *PRINT-READABLY* as a default value for the PKG-P
;; parameter to WRITE-SYMBOL, in this system - in lieu of a definition
;; of any additoinal globally scoped variable or generalized manner of
;; "Application Flag" singularly for affecting that behavior of this
;; system, per se.

(declaim (inline write-symbol
                 write-space))

(defun* write-symbol (s stream &optional pkg-p)
  ;; NB: If S is a string, it's assumed that any character in S such
  ;; that may have a particular interpretation in a symbol name, if
  ;; unescaped, will have already been escaped by the calling procedure.
  ;;
  ;; NB: This function will not perform any name mangling, vis a vis
  ;; character case.
  ;;
  ;; NB: This function - in effect, by way of PRIN1 - assumes that any
  ;; symbol exported when this function is called will also be exported
  ;; when the symbol name, as was printed to STREAM, is evaluated.
  ;;
  (declare (type symbol s) (type stream stream)
           (values stream &optional))
    (let ((*package* (if pkg-p
                         (mk-lf (find-package '#:keyword))
                         *package*)))
      ;; FIXME/TBD: When no symbol package, write an #<N>=<SYMBOL> expr
      ;; and store the symbol for <N> within the current PRINT-ENVIRONMENT
      (prin1 s stream))
    (values stream))

;; (with-output-to-string (s) (write-symbol 'frob s))
;; => "FROB"

;; (with-output-to-string (s) (write-symbol 'lambda s t))
;; => "COMMON-LISP:LAMBDA"

;; (with-output-to-string (s) (write-symbol ':keyword s t))
;; => ":KEYWORD"

;; (with-output-to-string (s) (write-symbol ':keyword s nil))
;; => ":KEYWORD"


(defun* write-space (stream)
  (declare (type stream stream)
           (values stream &optional))
  (write-char #\Space stream)
  (values stream))


(defgeneric write-reference-expression (expr stream)
  ;; NB: With some exceptions - such as denoted below - these method
  ;; definitions should not require the printed source forms to be
  ;; evaluated with any specific source system dependencies, insofar as
  ;; for the generic Comon Lisp expressions supported herein.

  ;; TBD
  ;; - HASH-TABLE (??)
  ;; - [...]

  (:method ((expr t) (stream symbol))
    (write-reference-expression expr (compute-output-stream stream))
    (values stream))

  (:method ((expr t) (stream stream))
    (warn "~<WRITE-REFERENCE-EXPRESSION dispatching to use MAKE-LOAD-FORM~>~
~< - Objects of type ~S not yet supported by WRITE-REFERENCE-EXPRESSION:~>~
~< ~S~>"
     (class-of expr) expr)
    (write-reference-expression (make-load-form expr) stream)
    (values stream))

  (:method ((expr symbol) (stream stream))
    ;; FIXME: Use a *PRINT-SYMBOL-PACKAGE* clause
    ;; - e.g '(and (not #:CL) (not #:keyword)) [default]
    (write-symbol expr stream *print-readably*)
    (values stream))

  (:method ((expr package) (stream stream))
    ;; NB: Writing the package identity -- assumed constant -- not any
    ;; form defining the package
    (write-char #\# stream)
    (write-char #\. stream)
    (write-reference-expression `(find-package ,(package-name expr))
                                stream)
    (values stream))

  (:method ((expr cons) (stream stream))
    (write-char #\( stream)
    ;; FIXME/TBD cf. *PRINT-CIRCLE*
    (do-cons (subexpr rest expr (values))
      (write-reference-expression subexpr stream)
      (when rest
        (write-space stream)
        ;; NB If REST is not a CONS, DO-CONS will not have bound it as SUBEXPR.
        ;; So, that's handled here - after which, DO-CONS will return.
        (unless (consp rest)
          (write-char #\. stream)
          (write-space stream)
          (write-reference-expression rest stream))))
    (write-char #\) stream)
    (values stream))

  (:method ((expr class) (stream stream))
    ;; Assumption: STREAM will be read with a readtable in which the
    ;; dispatching reader macro expression "#." is bound to a form
    ;; analogous that specified in CLtL2/CLHS
    (write-char #\# stream)
    (write-char #\. stream)
    (write-reference-expression `(find-class (quote ,(class-name expr))) stream)
    (values stream))

  (:method ((expr effective-slot-definition) (stream stream))
    ;; Assumption: The source forms will be evaluated within the same
    ;; Common Lisp implementation as in which the source forms were
    ;; printed. Thus, *PRINT-READABLY* will be expressly bound to T
    ;; here, such as to ensure that the symbol package for each symbol
    ;; in the implementation's MOP implementation will be printed as per
    ;; the package of that symbol's availability in the calling Lisp
    ;; environment.
    (write-char #\# stream)
    (write-char #\. stream)
    (let ((*print-readably* t))
      (write-reference-expression
       `(find (quote ,(slot-definition-name expr))
              ;; FIXME : 
              (class-slots ,(slot-definition-class expr))
              :test #'eq :key #'slot-definition-name)
       stream))
    (values stream))

  (:method ((expr function) (stream stream))
    (multiple-value-bind (lambda-expr closure-p name)
        (function-lambda-expression (compute-function expr))
      (declare (ignore closure-p)) ;; NB
      ;; NB: use PRIN1 for named functions,
      ;; Use WRITE-REFERENCE-EXPRESSION for anonymous lambda functions
      ;; for which the defining lambda forms are available,
      ;; and err for anonymous lambda functions with no "stored forms"
      (labels ((write-func-macro ()
                 (write-char #\# stream)
                 (write-char #\' stream)))
      (cond
        (name
         (write-func-macro)
         (write-reference-expression name stream))
        (lambda-expr
         (write-func-macro)
         (write-reference-expression lambda-expr stream))
        (t (simple-program-error
            "~<Cannot write anonymous lambda function absent of ~
stored definition forms:~>~< ~S~>" expr)))
      (values stream))))

  (:method ((expr character) (stream stream))
    ;; NB Ass'umption: The output stream has been initialized with an
    ;; external format such that will serve to support a character
    ;; encoding sufficient to represent every character and string
    ;; provided to WRITE-REFERENCE-EXPRESSION
    (princ expr stream)
    (values stream))

  (:method ((expr number) (stream stream))
    (prin1 expr stream)
    (values stream))

  (:method ((expr simple-array) (stream stream))
    (prin1 expr stream)
    (values stream))

  (:method ((expr array) (stream stream))
    (multiple-value-bind (displ displ-off-t)
        (array-displacement expr)
      #-NIL (declare (ignore displ-off-t))
      ;; NB Refer to FIXME/TBD annotation, below
      (when displ
        (simple-program-error
         "Displaced arrays not yet supported"))
      (let ((fp (when (array-has-fill-pointer-p expr)
                  (fill-pointer expr)))
            (adj (adjustable-array-p expr))
            (dim (array-dimensions expr))
            (typ (array-element-type expr))
            ;; NB: This generalized method may be fairly
            ;; resource-intensive when printing a "large array"
            (simple-contents (unless displ (coerce expr 'simple-array))))

        ;; NB: This application of "#." reader macro syntax, in a
        ;; manner, may be said to to represent a specialized case of
        ;; *PRINT-READABLY* := T
        (write-char #\# stream)
        (write-char #\. stream)
        (write-reference-expression
         `(make-array (quote ,dim)
                      ,@(when fp `(:fill-pointer ,fp))
                      ,@(when adj `(:adjustable ,adj))
                      :element-type (quote ,typ)
                      ,@(cond
                         (displ
                          ;; FIXME/TBD: Portable object
                          ;; references - specification and
                          ;; dereferencing - in this
                          ;; generalized PRINT-EXPR
                          ;; application
                          ;;
                          ;; NB *PRINT-CIRCLE* and #<N> syntax
                          #+NIL
                          `(:displaced-to
                            ,displ
                            :displaced-index-offset
                            ,displ-off-t))
                         (t
                          `(:initial-contents ,simple-contents))))
         stream)))
    (values stream))
  )

;; ----

;; FIXME - the name element, "Accessor," while it may be suitable in a
;; generalized regard, is too generic for denoting one of either a slot
;; value writer or slot value reader.

(defgeneric compute-reader-name (slot class)
  (:method ((slot standard-effective-slot-definition)
            (class standard-class))
    ;; Default method e.g for any class for which it would not be
    ;; assumed that any analogy to a DEFSTRUCT :CONC-NAME would be
    ;; available.
    (let ((sl-name (slot-definition-name slot))
          (typ-name (class-name class)))
      (concatenate 'simple-string (symbol-name typ-name)
                   "-" (symbol-name sl-name)))))


(defgeneric compute-reader-function-type (slot class)
  (:method ((slot standard-effective-slot-definition)
            (class standard-class))
    `(function (,(class-name class))
               (values ,(slot-definition-type slot) &optional))))


(defgeneric compute-reader-lambda (slot class)
  #+SBCL
  (:method ((slot standard-effective-slot-definition)
            (class standard-class))
    (let ((typname (class-name class)))
      (with-symbols (cls obj)
        `(lambda (,obj)
           (declare (type ,typname ,obj))
           ;; FIXME: standard objects typically cannot be printed readably.
           ;;
           ;; NB: Symbols can be bound within the lexical environment
           ;; in which the corresponding DEFUN form will be evaluated
           ;;
           ;; FIXME/TD: Bind *CLASS* and for each slot, *SLOT* in the
           ;; lexical environment in which this lambda form's body will
           ;; be evaluated, in the source forms produced from
           ;; PRINT-READER-FOR (??)
           ;; *OR* use reader macros in the printed representation
           ;; e.g #.(find-class (quote <class-name>))
           ;;     #.(find-slot (quote <slot-name>) (quote <class-name>))
           (let ((,cls ,class
                   #+NIL (load-time-value (find-class (quote ,typname))
                                          t)))
             (declare (dynamic-extent ,cls))
             (cond
               ((eq (class-of ,obj) ,cls)
                (assert-slot-boundp ,the-readable-slot ,obj)
                (standard-instance-access ,obj ,FIXME-TBD))
               (t
                (slot-value-using-class ,cls ,obj ,slot)))))))))


(defgeneric compute-reader-documentation (slot class)
  (:method ((slot standard-effective-slot-definition)
            (class standard-class))
    (format nil
            "Machine-Generated Reader for the ~A slot value of an ~A object"
            (slot-definition-name slot)
            (class-name class))))


(defgeneric print-reader-for (slot class stream)
  ;; NB - This protocol needs to dispatch for each slot value writer
  ;; and slot value reader, by default - without preventing that any
  ;; extending class may specify that a slot value writer would not be
  ;; written, or - per se - that any accessors would be "Skipped" for
  ;; any single slot in a single, defined class.
  ;;
  ;; NB: If an effective method - in effect - "Skips" a slot definition,
  ;; the effective method should return NIL. Otherwise, the effective
  ;; method should return a stream.
  (:method ((slot standard-effective-slot-definition)
            (class standard-class)
            (stream stream))
    (let ((name (compute-reader-name slot class))
          ;; TBD: Lambda-Printer protocol
          (lform (compute-reader-lambda slot class))
          (docstr (compute-reader-documentation slot class)))

      ;; NB: This function does not, at present, perform any "Pretty
      ;; Printed" indenting for printed source forms

      ;; write ftype decl
      (write-reference-expression
       `(declaim (ftype ,(compute-reader-function-type slot class)
                       name))
       stream)

      ;; write defun expr
      (write-char #\( stream)
      (write-symbol (quote defun) stream t)
      (write-space stream)
      (write-symbol name stream t)
      (write-space stream)
      ;;
      (destructuring-bind (lsym args &body lform-body) lform
        (declare (ignore lsym))
        (write-reference-expression args stream)
        (when docstr (print docstr stream))
        (dolist (expr lform-body)
          (write-reference-expression expr stream)
          (terpri stream)))
      ;;
      (write-char #\) stream)

      ;; NB: The method should call something like TERPRI itself,
      ;; thus in something of an analogy to PRINT
      (terpri stream)
      )))

(defun* write-accessors (class stream)
  (declare (type class-designator class)
           (type stream-designator stream)
           (values stream &optional))

  ;; TBD: Folding DEFUN calls into method forms, rather than expanding
  ;; to DEFUN source forms within a macroexpansion

  ;; Topic: Applying functional forms within a macroexpansion, using
  ;; MACROLET in the direct macroexpansion, such as to produce an
  ;; effective macroexpansion derived per values returned by those
  ;; functional forms.
  ;;
  ;; Alternate approach: "Indirect Eval," by way of source file
  ;; generation with functional procedures defined in Common Lisp source
  ;; code - towards a methodology for definition and appliation of
  ;; generalized source templates, in a manner after both CL:DEFMACRO
  ;; and CL:DEFCLASS. NB: This approach may serve to permit for review
  ;; of generalized "Template expansion" source files, without per se
  ;; MACROEXPAND-1 or MACROEXPAND -- whether or not it may clearly serve
  ;; to alleviate any concerns with regards to lexical bindings in the
  ;; compiler environment and lexical bindings in the macroexpansion
  ;; environment, as in applications of CL:DEFMACRO. This approach could
  ;; perhaps be anyhow influenced with considerations after definitions
  ;; in the SWIG system.

  (let ((%class (ensure-class-finalized-p class))
        (%stream (compute-output-stream stream)))
    (declare (type class %class) (type stream %stream))

    (terpri stream)
    ;; (write-char #\( stream)
    ;; (princ 'progn stream)

    ;; TBD: One of:
    ;; A) Write forms for lexical definition of *CLASS*.
    ;; B) Within each "Accessor printout", print a form such as
    ;;    #.(find-class (quote <class-name>))
    ;; .. to ensure generally static reference to the defining class,
    ;; within the generated accessor definitoins.

    (dolist (sl (class-slots %class))
      (declare (type effective-slot-definition sl))
      ;; FIXME - Similar to static class reference, ensure static slot
      ;; reference in each reader, writer form -- assuming that the
      ;; defining class will not be destructively redfined, subsequent
      ;; of these generalized "Print" calls.

      ;; NB: Each of these reader, writer printer forms should also
      ;; print an FTYPE declaration to the stream.
      (print-reader-for sl class stream)
      (print-writer-for sl class stream))

     ;; (write-char #\) stream)
     ;; (terpri stream)

    (values %stream)))
