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
;; Generalized unparser functions
;;

(declaim (inline princ-symbol
                 princ-space))

(defun* princ-symbol (s stream &optional pkg-p)
  ;; NB: If S is a string, it's assumed that any character in S such
  ;; that may be "Special" for a symbol name will have already been
  ;; escaped by the calling procedure.
  ;;
  ;; NB: This function will not perform any name mangling, vis a vis
  ;; character case.
  ;;
  ;; NB: When PKG-P
  ;;  - This function will not access any packgage nicknames
  ;;  - This function will not assume that any symbol is exported
  ;;
  (declare (type (or symbol string) s) (type stream stream)
           (values stream &optional))
  (macrolet ((the-string (expr)
               (with-symbols (%expr)
                 `(let ((,%expr ,expr))
                    (etypecase ,%expr
                      (symbol (symbol-name ,%expr))
                      (string ,%expr)))))
             (the-pkg-string (expr)
               (with-symbols (%expr)
                 `(let ((,%expr ,expr))
                    (etypecase ,%expr
                      (symbol (package-name (symbol-package ,%expr)))
                      (t (package-name *package*)))))))
    (when  pkg-p
      (princ (the-pkg-string s) stream)
      (write-char #\: stream) (write-char #\: stream))
    (princ (the-string s) stream)
    (values stream)))

;; (with-output-to-string (s) (princ-symbol 'frob s))
;; => "FROB"

;; (with-output-to-string (s) (princ-symbol 'lambda s t))
;; => "COMMON-LISP::LAMBDA"


(defun* princ-space (stream)
  (declare (type stream stream)
           (values stream &optional))
  (write-char #\Space stream)
  (values stream))

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
           (let ((,cls ,class
                   #+NIL (load-time-value (find-class (quote ,typname))
                                          t)))
             (declare (dynamic-extent ,cls))
             (cond
               ((eq (class-of ,obj) ,cls)
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
      (write-char #\( stream)
      (princ-symbol (quote declaim) stream t)
      (princ-space stream)
      (write-char #\( stream)
      (princ-symbol (quote 'ftype) stream t)
      (princ-space stream)
      (print (compute-reader-function-type slot class) stream)
      (princ-symbol name stream t)
      (write-char #\) stream)
      (write-char #\) stream)
      (terpri stream)
      (terpri stream)

      ;; write defun expr
      (write-char #\( stream)
      (princ-symbol (quote defun) stream t)
      (princ-space stream)
      (princ-symbol name stream t)
      (princ-space stream)
      ;;
      (destructuring-bind (lsym args &body lform-body) lform
        (declare (ignore lsym))
        (print args stream)
        (when docstr (print docstr stream))
        (dolist (expr lform-body)
          (print expr stream)))
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

    (dolist (sl (class-slots %class))
      (declare (type effective-slot-definition sl))
      (when (print-reader-for sl class stream)
        (terpri stream))
      (when (print-writer-for sl class stream)
        (terpri stream)))

     ;; (write-char #\) stream)
     ;; (terpri stream)

    (values %stream)))
