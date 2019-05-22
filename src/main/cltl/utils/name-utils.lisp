;; name-utils.lisp - symbol/name utilities
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - API and initial implementation
;;
;;------------------------------------------------------------------------------

;; FIXME The annotations in this file obviously need to be managed in something like documentation

(in-package #:ltp-utils)

(eval-when (:compile-toplevel :eval)
  (defmacro array-mkproto (arr)
    (with-symbols (%arr)
      `(let ((,%arr ,arr))
         (cond
           ((adjustable-array-p ,%arr)
            (make-array (array-dimensions ,%arr)
                        :element-type (array-element-type ,%arr)
                        :adjustable t
                        :fill-pointer (fill-pointer ,%arr)))
           (t
            (make-array (array-dimensions ,%arr)
                        :element-type (array-element-type ,%arr)
                        :adjustable nil)))))))

(defun string-invert-case (str)
  "Return a copy of STR such that every letter charater in STR has an
inverted character case"
  (let* ((len (length str))
         (strcp (unless (zerop len) (array-mkproto str))))
    (declare (type array-dim len))
    (cond
      ((zerop len) (copy-seq str))
      (t
       (dotimes (n len strcp)
         (let ((c (char str n)))
           (setf (aref strcp n)
                 (cond
                   ((upper-case-p c)
                    (char-downcase c))
                   ((lower-case-p c)
                    (char-upcase c))
                   (t c)))))))))

;; (string-invert-case "nAME")
;; => "Name"
;;
;; (string-invert-case "NAME")
;; => "NAME"
;;
;; (string-invert-case "name-2.4-8")
;; => "NAME-2.4-8"
;;
#-(and)
(let* ((initial "name-2.4-8")
       (buff
        (make-array 16 :element-type 'base-char :fill-pointer 10)))
  (dotimes (n (length initial))
    (setf (char buff n) (schar initial n)))
  (let ((ret (string-invert-case buff)))
    (values ret
            (fill-pointer ret)
            (array-dimensions ret))))
;; => "NAME-2.4-8", 10, (16)

(defun nstring-invert-case (str)
  "Destructively modify STR such that every letter charater in STR will
have a character case inverted to its orignal value.

The object STR will be returned"
  (let ((len (length str)))
    (declare (type array-dim len))
    (cond
      ((zerop len) (values str))
      (t
       (dotimes (n len str)
         (let ((c (char str n)))
           (setf (aref str n)
                 (cond
                   ((upper-case-p c)
                    (char-downcase c))
                   ((lower-case-p c)
                    (char-upcase c))
                   (t c)))))))))

;; (nstring-invert-case "NaMe")
;; => "nAmE"


(defun mangle-case (str case)
  ;; FIXME/TD Define a "destructively modifies" form
  ;;
  ;; -- requires  [add'l to nstring-invert-case]
  ;;    - nstring-downcase
  ;;    - nstring-upcase
  ;;    - nstring-capitalize (NB: Parser Hacks, or RATHER Pretty-Printer
  ;;      hacks and quirks of the disjunction of CL #'READTABLE-CASE and
  ;;      CL *PRINT-CASE* if only insofar as affecting behaviors of
  ;;      prin1/prin1-to-string i.e print-readable-string/print-escaped-string
  ;;      for SYMBOL type values - also w/ NB side effects of any
  ;;      *PRINT-READABLY* bindings, such that establish a really
  ;;      troubling set of requirements FOR portable PRINT-OBJECT methods)
  ;;
  ;;      - NB: Three-state logic for character case onto CL
  ;;            Upcase / Downcase / Nonchar
  ;;      - NB: :UPCASE state in readtable => Wrap in || if name is not
  ;;             all upcase / similar for :DOWNCASE / N/A for :PRESERVE
  ;;             TBD for :INVERT
  ;;
  ;; +- and document these, in a "Names Dictionary" for LTP
  ;;    - w/ xref in an introduction to the LTP manual, in which "Forms
  ;;      for destructive modification of sequence values" may be
  ;;      introduced - narrowly any confusion about pointers to pointers
  ;;      in C type decls, and pointer dereferencing in C applications
  ;;      ... and variables as lexically scoped metaobjects in Common Lisp.
  ;;
  ;; SEE ALSO: LIBREOFFICE; UNO SDK b.c DOCBOOIK and TEI P5
  ;;
  ;; NB - Examples - CASE may be provided by #'READTABLE-CASE or *PRINT-CASE*
  ;;      but note the slightly different syntax and semantics of which, namely:
  ;;       - #'READTABLE-CASE - unique :INVERT specifier
  ;;       - *PRINT-CASE* - unique :CAPITALIZE specifier
  (declare (type string str)
           (type (member :upcase :downcase :preserve :invert :capitalize) case)
           (values string))
  (ecase case
    (:upcase (values (string-upcase str)))
    (:downcase (values (string-downcase str)))
    (:preserve (values (copy-string str)))
    (:capitalize (values (string-capitalize str)))
    (:invert (values (string-invert-case str)))))


#| NB - CLSYS QA - Notice how things apart fall when *PRINT-READABLY*

Example:

 A) after
     (setq *print-readably* t)
    in SLIME

 B) subsq of (find-symbol '<non-string-value>)

 This:

 #<TYPE-ERROR expected-type: STRING datum: <NON-STRING-VALUE>> cannot be printed readably.
   [Condition of type PRINT-NOT-READABLE]

--

 NB MAKE-LOAD-FORM would not be applicable here.

  Notice that the OBJECT, in this example -- that for which the
  unadorned CLtl2 implementation cannot produce a printable
  representation -- that it is an instance of a condition type. Although
  an equivalent condition type could be produced, any such "New Object"
  would not be EQ to the actual object for which a readable
  representation is to be printed.

  Here, as such, we may have an example of how the definition of the
  semantics of the interactive Lisp REPL in Common Lisp specifications
  has ultimately obscured the -- in a phrase -- the "hairy underside" of
  memory management in Common Lisp applications.


TBD: :AROUND methods for PRINT-OBJECT, dispatching locally when the object
 is denoted as to have an "Unreadable" printed represntation && *PRINT-READABLY*

 - NB: These :AROUND methods might be called from within
   PRINT-UNREADABLE-OBJECT forms

 - NB: Printing a raw memory reference, for any single Common Lisp
   application, may sometimes be completely advisable. (!) As to how any
   such "raw memory reference" may be safely dereferenced - not
   disregarding the behaviors of arbitrary GC threads - a novel
   challenge for language development in Common Lisp, if not furthermore
   for Systems Development in the UNIX applications environment.

   Note, inasmuch - towards dereferncing a raw memory pointer onto the
   Common Lisp heap or Common Lisp stack, in any single Common Lisp
   implementation:

   - Reference Object's Type - as may be displayed with PRINT-UNREADABLE-OBJECT

   - Reference Object's Address - similarly, insofar as how most (??)
      Common Lisp implementations handle the :IDENTITY arg for
      PRINT-UNREADABLE-OBJECT


----

   And thus note - as inherited to SBCL via

  (but NB this might seem very non-trivial to port onto e.g CCL, if not also MKCL, ECL, ...)

   - SB-KERNEL:GET-LISP-OBJ-ADDRESS
   - SB-KERNEL:MAKE-LISP-OBJ - AND NOTE THE SOURCE COMMENTS NEAR THIS' <DEFUN>. cf GC IN SBCL
     ^ NB Uses intrinsict low-end-type-by-address semantics (per CMUCL, SBCL impl)
   - et c.

   EXAMPLE

   (sb-kernel:make-lisp-obj (sb-kernel:get-lisp-obj-address (find-class 'standard-class)))

   - NB SAP-STACK, CONTROL-STACK, UNSIGNED-STACK, ... in SBCL VOPs (e.g debug.lisp)

   - TOPIC (LTP INTERNALS KB - CMUCL, SBCL) : System Area Pointers and *CL xalloc

-- ^ NB: Insofar as during the evaluation of any PRINT-OBJECT method,
   any OBJECT provided to that method may be assumed to be at least
   "Still Available" within the duration of the evaluation of that
   method.

-- NB: Type-Tagged Memory Management in CMUCL and SBCL

  ... and that in CCL see e.g
   /usr/local/lib/ccl/compiler/X86/X8664/x8664-arch.lisp
   /usr/local/lib/ccl/compiler/ARM/arm-arch.lisp
   ^ in which it looks like a substantially different (?!) perhaps much more
    condensed (??) kernel than in CCL x86:64
  ... WHICH MAY SERVE TO REITERATE THE UTILITY OF UML+SYSML MODELS FOR COMMON LISP

 - see also: {...}

----

 TBD: #'CCL:EGC - note CCL x86-utils.lisp, arm-utils.lisp (ASM exprs in a Scheme-like langage)
----

 NB: CCL X8664::*X8664-TARGET-ARCH* [source code]
 ^ note some *-offset slots

 NB (type-of x8664::*x8664-subprims*)
  => (SIMPLE-VECTOR 154)

 NB <SP> in CCL => <subprim>

 NB (?) (type-of x8664::*x8664-subprims-shift*)
  => (INTEGER 0 1152921504606846975)

        x8664::*x8664-subprims-shift*
  => 3

        x8664::x8664-subprims-base
  => #x15000

 NB every keyword bucket in x8664::*x8664-target-uvector-subtags*
    such that maps to a class name in #:CL (or elsewhere) - and those few
    that do not

  e.g (let ((clp (find-package '#:ccl)) (*print-base* 16))
           (dolist (bucket x8664::*x8664-target-uvector-subtags* nil)
             (destructuring-bind (s . v) bucket
               (unless (find-class (intern (symbol-name s) clp) nil)
                 (print (list s v) t)))))
  % ^ NB This simple form uses INTERN not FIND-SYMBOL

    (:STRUCT 36)
    (:ISTRUCT 46)
    (:HASH-VECTOR 35)
    (:INSTANCE 86)
    ;; NB
    (:SIGNED-8-BIT-VECTOR D7)
    (:UNSIGNED-8-BIT-VECTOR E7)

    (:SIGNED-16-BIT-VECTOR A7)
    (:UNSIGNED-16-BIT-VECTOR B7)

    (:SIGNED-32-BIT-VECTOR D9)
    (:UNSIGNED-32-BIT-VECTOR E9)

    (:SIGNED-64-BIT-VECTOR DA)
    (:UNSIGNED-64-BIT-VECTOR EA)
    ;; NB
    (:VECTOR-HEADER A6)
    (:ARRAY-HEADER A5)
    ;; SUBSQ
    (:MIN-CL-IVECTOR-SUBTAG 97)

----
 TBD: 32-bit DSO ABI interop onto CCL, SBCL, ...

 TBD: 32-bit ELFs and dlsym() w/i a 64-bit "THING"

----

 NB (?) static locations in FASL encodings

----

Note the many PRINT-OBJECT methods defined in <OpenMCL as CCL>

Note also, the concept of a "LAP" in <OpenMCL as CCL> (Lisp Address Pointer??)

Orthogonally note (apropos "addr" '#:ccl)
  - more specifically:

   (defun print-fns (name &optional (package '#:ccl))
   (dolist (fn (mapcan #'(lambda (s) (when (fboundp s) (list (fdefinition s)))) (apropos-list name  package)))
           (print fn t)))

    (print-fns "addr")
    (print-fns "obj")
    (print-fns "macptr")
    (print-fns "offset")

    (print-fns "acode") ;; cf. CCL FASL gen (??) NB CCL FRAG objs (??)

    (print-fns "ptr")

 - NB Novelty CCL #(ADDR <THING>) syntax during PRINT of a FUNCTION
   there ("addr" search, above)

 - note thus: CCL::%ADDRESS-OF (CCL x86:64, ARM, ...)
   NB:
   /usr/local/lib/ccl/level-0/X86/x86-utils.lisp
   /usr/local/lib/ccl/level-0/ARM/arm-utils.lisp

 -- NB (CCL::NX-LOOKUP-TARGET-UVECTOR-SUBTAG :MACPTR)
 -- NB CCL X8664::DEFINE-SUBTAG

 !- but HOWTO: ADDRESS to OBJECT with CCL ?

 ... d dh d d d ddhf

  (ccl:%address-of (find-class 'standard-class))
  ^ insofar at least as for handling the :IDENTITY arg w/ PRINT-UNREADABLE-OBJECT
  ^ note that this function is passingly documented in debugging.ccldoc
    and it may not be tractably usable for much, without some more
    infrastructure "Low level" operations with CCL (namely for operns with
    "Intermediate Value" types in CCL)

  WOULDN'T IT BE NICE IF THEY'D DESCRIBED THE CCL DESIGN FOR ANYONE ELSE TO READ.

  EVEN A COMPLEAT UML MODEL OF THE SOURCE CODE MAY NOT BE NEARLY ENOUGH.


  SO, OTHERWISE FOCUSING ON PORTABLE SYMOBLIC REFERENCES ONTO COMMON LISP PROGRAMS

 -- (??) --
  (ccl:%int-to-ptr (ccl:%address-of (find-class 'standard-class)))
  TO DERERENCE IT TO THE ORIGINAL OBJ ? FROM A CCL MACPTR OBJ ?

  N/A for this - CCL::%GET-PTR, CCL::SAFE-GET-PTR

  an interned symbol: CCL::DEREF-MACPTR but not useful w/o (????)

  -NB- defun of CCL::STANDARD-OBJECT-P

 -- [...] --

  NB (CCL::ROOM)
  ... in a multi-threaded CCL build

 -- [.!...] --

 NB /usr/local/lib/ccl/library/lispequ.lisp
 in which CCL's adoption of the CMUCL Python compiler's type system is
 openly referenced - TYPE-CLASS, CTYPE, and subsq (w/ a lot of %SVREF ??)

 -- PEVIOUSLY --

  (CCL::%GET-OBJECT MACPTR?? OFFSET??)

   ^ OFFSET is an offset onto what again ?

   ? TBD CCL::WITH-MACPTRS

   ? NB (print-unreadable-object ((CCL:%NULL-PTR) t :type t :identity t))
   ?? NB (ccl:%int-to-ptr 0)

   ?!
   (ccl:%int-to-ptr (ccl:%address-of (find-class 'standard-class)))

   ?! so what about the illustrious offset for %GET-OBJECT ? Irrelvant here ??

   ?? NB  "The MACTPR type"
      in /usr/local/lib/ccl/lib/foreign-types.lisp
      it looks so much like CMUCL's ALIEN system. Go figure?

   ^ NB in x86 CCL IMPL - ASM (??)
    (trap-unless-lisptag= offset target::tag-fixnum imm1)

  ^ NB: a numeric address in CCL is not usable as a MACPTR.
     - a CCL MACPTR is some kind of a structured object, though it's
       absolutely unclear as to how any such object is ever created in CCL.

 - NB QA W/ CCL - CCL:*RECORD-SOURCE-FILE*, CCL:*SAVE-SOURCE-FILE-LOCATIONS*,
    CCL:*RECORD-PC-MAPPING*

 - NB QA W/ CCL - (CCL:FUNCTION-SOURCE-NOTE #'PRINT)

  But it would be so simple if all implementations were CMUCL or SBCL. (??)


 - NB in CCL x8664-arch.lisp [structured strawpile]

   "There are two kinds of macptr; use the length field of the header if
   you need to distinguish between them"

   subsq: MACPTR, XMACPTR

--

 TBD: Meaning of source comments for CCL::ALL-OBJECTS-OF-TYPE


 NB

 (defun low-types ()
           (let ((typel (list nil)))
             (ltp-utils:do-vector (s ccl::*heap-utilization-vector-type-names* (cdr typel))
               (unless (or (null s) (eq s 'ccl::bogus))
                 (rplacd (last typel)
                         (cons s nil))))))

  CL-USER> (low-types)
  (SYMBOL RATIO BIGNUM MACPTR CCL::CATCH-FRAME COMPLEX DOUBLE-FLOAT
  CCL::DEAD-MACPTR CCL::HASH-VECTOR STRUCTURE CCL::XCODE-VECTOR
  CCL::POOL CCL::INTERNAL-STRUCTURE CCL::COMPLEX-SINGLE-FLOAT POPULATION
  CCL::VALUE-CELL CCL::COMPLEX-DOUBLE-FLOAT PACKAGE CCL::XFUNCTION
  CCL::SLOT-VECTOR LOCK CCL::BASIC-STREAM CCL::INSTANCE FUNCTION
  CCL::SIMPLE-COMPLEX-DOUBLE-FLOAT-VECTOR CCL::ARRAY-HEADER
  CCL::VECTOR-HEADER CCL::SIMPLE-SIGNED-WORD-VECTOR SIMPLE-VECTOR
  CCL::SIMPLE-UNSIGNED-WORD-VECTOR
  CCL::SIMPLE-COMPLEX-SINGLE-FLOAT-VECTOR SIMPLE-BASE-STRING
  CCL::SIMPLE-FIXNUM-VECTOR CCL::SIMPLE-SIGNED-BYTE-VECTOR
  CCL::SIMPLE-SIGNED-LONG-VECTOR CCL::SIMPLE-SIGNED-DOUBLEWORD-VECTOR
  CCL::SIMPLE-UNSIGNED-BYTE-VECTOR CCL::SIMPLE-UNSIGNED-LONG-VECTOR
  CCL::SIMPLE-UNSIGNED-DOUBLEWORD-VECTOR BIT-VECTOR
  CCL::SINGLE-FLOAT-VECTOR CCL::DOUBLE-FLOAT-VECTOR)


 and NB CCL::ALL-OBJECTS-OF-TYPE (TYPE PRED) ;; feat. source code commentary

 AAAND NB CCL::%MAP-AREAS [used in CCL::ALL-OBJECTS-OF-TYPE]


 NB #'CCL::TYPECODE - internal meaning cf. all-objects-of-type SRC FORM

 e.g  (ccl::typecode (find-class 'standard-class))
  => 134

--

 TBD (DEFLOWTYPE PODLE #:WOFF #'MAKE-WOFF #'FREE-WOFF #'WOFFUNDEF)

 (PRINT-UNBARKABLE-OBJECT (ALLOC 'PODLE) *WRONG-TREE*)

--

 NB FFI with CCL - CCL::SETUP-X8664-FTD (CCL foreign type data)

 NB FFI with CCL - CCL:%FF-CALL (arch-specific impl)

--

 NB /usr/local/lib/ccl/level-1/l1-clos-boot.lisp

--
...
/usr/local/lib/ccl/compiler/subprims.lisp
- CCL::SUBPRIMITIVE-INFO
/usr/local/lib/ccl/compiler/X86/x86-lap.lisp
- TBD Vectorization (??) and CCL
- NB machine-specific extension of CCL::DLL-NODE w/ CCL::X86-LAP-NOTE & subtypes
  - see also /usr/local/lib/ccl/compiler/ARM/arm-lap.lisp
  - principally see also /usr/local/lib/ccl/compiler/dll-node.lisp
    ^ NB DLL => Doubly Linked List (UTIL)
/usr/local/lib/ccl/compiler/backend.lisp (??)
/usr/local/lib/ccl/compiler/nx-basic.lisp
/usr/local/lib/ccl/compiler/nx0.lisp
-(NB REFLX w/ CCL ??)
  /usr/local/lib/ccl/lib/source-files.lisp
  /usr/local/lib/ccl/compiler/X86/x86-disassemble.lisp
-(NB FFI w/ CCL)
 /usr/local/lib/ccl/lib/foreign-types.lisp
 /usr/local/lib/ccl/lib/db-io.lisp
-(NB MOP w/ CCL)
 /usr/local/lib/ccl/lib/method-combination.lisp


/usr/local/lib/ccl/lib/hash.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS HASH-TABLE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-unicode.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CHARACTER-ENCODING> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-streams.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::IO-BUFFER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::BASIC-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SYNONYM-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS TWO-WAY-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS STRING-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::FD-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VECTOR-STREAM> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/linux-files.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS EXTERNAL-PROCESS> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-files.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS PATHNAME> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-typesys.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TYPE-CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::CTYPE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TYPE-CELL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/sysutils.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::SPARSE-VECTOR> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-lisp-threads.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::LISP-THREAD> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-processes.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS PROCESS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS LOCK> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS LOCK-ACQUISITION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS SEMAPHORE-NOTIFICATION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-io.lisp
  (:METHOD PRINT-OBJECT :AROUND (#<BUILT-IN-CLASS T> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS T> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CHARACTER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS INTEGER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RATIO> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS COMPLEX> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS FLOAT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VALUE-CELL> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS STANDARD-OBJECT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS STANDARD-METHOD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METHOD-FUNCTION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<FUNCALLABLE-STANDARD-CLASS STANDARD-GENERIC-FUNCTION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METAOBJECT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::COMBINED-METHOD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SLOT-DEFINITION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS EQL-SPECIALIZER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::SLOT-ID> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TAGGED-RETURN-ADDRESS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::SYMBOL-VECTOR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::FUNCTION-VECTOR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::CLASS-CELL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-reader.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::PACKAGE-REF> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS SOURCE-NOTE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-error-system.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RESTART> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CONDITION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-events.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::PERIODIC-TASK> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-sysio.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS EXTERNAL-FORMAT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::FUNDAMENTAL-FILE-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::BASIC-FILE-STREAM> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/numbers.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RANDOM-STATE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/subprims.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::SUBPRIMITIVE-INFO> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/X86/x86-lap.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::X86-LAP-LABEL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/backend.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::BACKEND> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/nx-basic.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::CODE-NOTE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::ACODE-AFUNC-REF> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::ACODE-REF> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/nx0.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VAR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::ACODE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/source-files.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS DEFINITION-TYPE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/X86/x86-disassemble.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::X86-DISASSEMBLED-INSTRUCTION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/foreign-types.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::INTERFACE-DIR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::FOREIGN-TYPE-CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS EXTERNAL-ENTRY-POINT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::FOREIGN-VARIABLE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS SHLIB> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/db-io.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::CDBX> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::FFI-TYPE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::OBJC-METHOD-INFO> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/method-combination.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METHOD-COMBINATION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/pprint.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::PPRINT-DISPATCH-TABLE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/describe.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::UNBOUND-MARKER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::BOGUS-OBJECT-WRAPPER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::ERROR-FRAME> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::STACK-INSPECTOR> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/library/remote-lisp.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::REMOTE-LISP-THREAD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::REMOTE-LISP-CONNECTION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/library/sockets.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::SOCKET-ADDRESS> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/tools/asdf.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/COMPONENT:COMPONENT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-COMPONENT:MISSING-DEPENDENCY> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-SYSTEM:MISSING-COMPONENT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-COMPONENT:MISSING-COMPONENT-OF-VERSION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/OPERATION:OPERATION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/ACTION:ACTION-STATUS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/PLAN:PLANNED-ACTION-STATUS> #<BUILT-IN-CLASS T>))
/usr/home/gimbal/.emacs.d/libcl/slime/swank.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SWANK::CHANNEL> #<BUILT-IN-CLASS T>))
/usr/home/gimbal/.emacs.d/libcl/slime/contrib/swank-trace-dialog.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SWANK-TRACE-DIALOG::TRACE-ENTRY> #<BUILT-IN-CLASS T>))/usr/local/lib/ccl/lib/hash.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS HASH-TABLE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-unicode.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CHARACTER-ENCODING> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-streams.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::IO-BUFFER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::BASIC-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SYNONYM-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS TWO-WAY-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS STRING-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::FD-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VECTOR-STREAM> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/linux-files.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS EXTERNAL-PROCESS> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-files.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS PATHNAME> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-typesys.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TYPE-CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::CTYPE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TYPE-CELL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/sysutils.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::SPARSE-VECTOR> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-lisp-threads.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::LISP-THREAD> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-processes.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS PROCESS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS LOCK> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS LOCK-ACQUISITION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS SEMAPHORE-NOTIFICATION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-io.lisp
  (:METHOD PRINT-OBJECT :AROUND (#<BUILT-IN-CLASS T> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS T> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CHARACTER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS INTEGER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RATIO> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS COMPLEX> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS FLOAT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VALUE-CELL> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS STANDARD-OBJECT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS STANDARD-METHOD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METHOD-FUNCTION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<FUNCALLABLE-STANDARD-CLASS STANDARD-GENERIC-FUNCTION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METAOBJECT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::COMBINED-METHOD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SLOT-DEFINITION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS EQL-SPECIALIZER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::SLOT-ID> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::TAGGED-RETURN-ADDRESS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::SYMBOL-VECTOR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::FUNCTION-VECTOR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::CLASS-CELL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-reader.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::PACKAGE-REF> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS SOURCE-NOTE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-error-system.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RESTART> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CONDITION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-events.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::PERIODIC-TASK> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/level-1/l1-sysio.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS EXTERNAL-FORMAT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::FUNDAMENTAL-FILE-STREAM> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::BASIC-FILE-STREAM> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/numbers.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS RANDOM-STATE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/subprims.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::SUBPRIMITIVE-INFO> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/X86/x86-lap.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::X86-LAP-LABEL> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/backend.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::BACKEND> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/nx-basic.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::CODE-NOTE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::ACODE-AFUNC-REF> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::ACODE-REF> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/nx0.lisp
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::VAR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::ACODE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/source-files.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS DEFINITION-TYPE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/compiler/X86/x86-disassemble.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::X86-DISASSEMBLED-INSTRUCTION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/foreign-types.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::INTERFACE-DIR> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::FOREIGN-TYPE-CLASS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS EXTERNAL-ENTRY-POINT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS CCL::FOREIGN-VARIABLE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<BUILT-IN-CLASS SHLIB> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/db-io.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::CDBX> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::FFI-TYPE> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::OBJC-METHOD-INFO> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/method-combination.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS METHOD-COMBINATION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/pprint.lisp
  (:METHOD PRINT-OBJECT (#<STRUCTURE-CLASS CCL::PPRINT-DISPATCH-TABLE> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/lib/describe.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::UNBOUND-MARKER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::BOGUS-OBJECT-WRAPPER> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::ERROR-FRAME> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS INSPECTOR::STACK-INSPECTOR> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/library/remote-lisp.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::REMOTE-LISP-THREAD> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::REMOTE-LISP-CONNECTION> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/library/sockets.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS CCL::SOCKET-ADDRESS> #<BUILT-IN-CLASS T>))
/usr/local/lib/ccl/tools/asdf.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/COMPONENT:COMPONENT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-COMPONENT:MISSING-DEPENDENCY> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-SYSTEM:MISSING-COMPONENT> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/FIND-COMPONENT:MISSING-COMPONENT-OF-VERSION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/OPERATION:OPERATION> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/ACTION:ACTION-STATUS> #<BUILT-IN-CLASS T>))
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS ASDF/PLAN:PLANNED-ACTION-STATUS> #<BUILT-IN-CLASS T>))
/usr/home/gimbal/.emacs.d/libcl/slime/swank.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SWANK::CHANNEL> #<BUILT-IN-CLASS T>))
/usr/home/gimbal/.emacs.d/libcl/slime/contrib/swank-trace-dialog.lisp
  (:METHOD PRINT-OBJECT (#<STANDARD-CLASS SWANK-TRACE-DIALOG::TRACE-ENTRY> #<BUILT-IN-CLASS T>))

|#
