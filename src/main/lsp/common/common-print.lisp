;; print-utils.lisp - utilities for object printing
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

;; FIXME_DOCS - document the generic functions and methods defined
;; here. Refer to subsequent commentary, this file


;; FIXME_DESIGN - Reconsider the ad hoc design of this API. Considerations:
;; - 'Object Label' as a short-form Object Identity string
;;
;;    - principally designed for use with "The normal REPL printer" as
;;      in regards to the "Name" part of object identity printouts for
;;      unreadable objects.
;;
;;    - may also be reused within MAKE-LOAD-FORM forms, but note that
;;      these would need a management API to ensure a sense of
;;      managability to the same, principally for the Common Lisp
;;      systems developer (in FOSS environments)
;;
;; - 'Object Name' as a long-form Object Identity string
;;
;;    - principally designed for use in "Pretty Printing" applications,
;;      in console and desktop environments
;;
;; - "Pretty Printer" as a novel concept (Shoutout to the Multics developers)
;;
;;    - principally towards utilizing the 'Object Name' of an object as  "When Pretty Printing".
;;
;;      - note the ANSI CL/CLtL2 API for Pretty Printing, namely concerning global variables designed for "The Pretty Printer"
;;
;;    - furthermore towards "Pretty Printing" in graphical desktop environments.
;;
;;      - see also: Garnet KR
;;


(in-package #:ltp/common)


;;
;; Note the class PRETTY-PRINTABLE-OBJECT
;;
(defgeneric (setf object-print-name) (new-value object))
(defgeneric (setf object-print-label) (new-value object))


#|

 The functions PRINT-NAME an PRINT-LABEL, and the functions
 OBJECT-PRINT-NAME and OBJECT-PRINT-LABEL, may be applied
 interchangably -- respectively, for printing an object's name or label
 to a stream, or for returning an object's name or label as a simple
 string value.


 For an object of a type that is a subclass of PRETTY-PRINTABLE-OBJECT,
 the functions OBJECT-PRINT-NAME and OBJECT-PRINT-LABEL, respectively,
 may dispatch to slot-value accessors methods specialized onto the
 PRETTY-PRINTABLE-OBJECT, unless the accessor methods are overridden.

 Typically, an object's print-label would represents a succinct
 identity of the object, whereas an object's print-name would
 represent a less succinct identity.

 Examples

  OBJECT-PRINT-NAME PACKAGE returns the name of the package

  OBJECT-PRINT-LABEL PACKAGE returns the shortest string among the
    name and nicknames of the package

  PRINT-NAME SYMBOL STREAM, for a symbol interned in a package, in
     will apply the value returned by OBJECT-PRINT-NAME, in printing
     the name of the package of the symbol.

  PRINT-NAME SYMBOL STREAM, for a symbol interned in a package, in
     will instead apply the value returned by OBJECT-PRINT-LABEL, in
     printing the name of the package of the symbol.


 The PRINT-NAME and PRINT-LABEL methods may be more readily useful,
 for instances in which an object's name represents the structural
 quaities of an object, such that may be indivdiually printed to an
 output stream string for constructing the structured name or label of
 the object.

 OBJECT-PRINT-NAME and OBJECT-PRINT-LABEL, altrenatey, may be most
 useful for objects of which a single string represents each of the
 object's name and label qualities -- for instance, for the
 measurement unit, MATH::METER, the print name of the object is
 the simple string, "meter", whereas the print lable of the object is
 the simple string, "m"

 An example of the application of this protocol is developed in the
 Igenous-Math system -- noting, in furthermore, the methods

   PRINT-LABEL (MATH:MEASUREMENT)
   PRINT-LABEL (MATH:KILOGRAM)

 FIXME: For a brief time, a similar protocol was being designed onto
 the Dobelle-App source tree, namely in the class LABELED-OBJECT
 Pending further updates to that source tree, that protoocl should
 be revised to align with the present pretty printing protocol, in
 conventional stream-io proedures, in CLIM presentation methods, and
 in other CLIM stream output procedures.


|#

(defgeneric print-name (object stream)
  (:method (object (stream symbol))
    (print-name object
		(ecase stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*))))
  (:method (object (stream stream))
    (prin1 object stream))
  (:method ((object package) (stream stream))
    ;; prefer package name
    (print-name (object-print-name object) stream))
  (:method ((object class) (stream stream))
    (print-name (class-name object) stream))
  (:method ((object function) (stream stream))
    (print-name (function-name object) stream))
  (:method ((object symbol) (stream stream))
    ;; Ed. NB: Note the specific handling, here. Reusable, moreover,
    ;; for printing any Common Lisp symbol onto any STREAM
    (multiple-value-bind (status pkg)
	(symbol-status object)
      (let ((keyword-p
	     (and pkg (eq pkg (find-package '#:keyword)))))
	(cond
	  (keyword-p
	   (write-char #\: stream))
          ;; NB : may fall through to PRINT-NAME for PKG
	  (pkg (princ (object-print-name pkg) stream))
	  (t (write-char #\# stream)
	     (write-char #\: stream)))
	(case status
	  (:external
	   (unless keyword-p
	     (write-char #\: stream)))
	  ((:internal :inherited)
	   (write-char #\: stream)
	   (write-char #\: stream)))
	(write-string (symbol-name object) stream))))
  )

;; (with-output-to-string (*terminal-io*) (print-name (find-class 'string) t))
;; => "COMMON-LISP:STRING"

(defgeneric print-label (object stream)
  (:method (object (stream symbol))
    (print-label object
		(ecase stream
		  ((t) *terminal-io*)
		  ((nil) *standard-output*))))
  (:method (object (stream stream))
    (princ object stream))
  (:method ((object package) (stream stream))
    ;; prefer package short name - cf the corresponding method
    ;; onto OBJECT-PRINT-LABEL
    (print-label (object-print-label object) stream))
  (:method ((object class) (stream stream))
    (print-label (class-name object) stream))
  (:method ((object function) (stream stream))
    (print-label (function-name object) stream))
  (:method ((object symbol) (stream stream))
    ;; FIXME - this duplicates PRINT-NAME (SYMBOL STREAM)
    ;; in lieu of making any nested call to PRINT-NAME. The main
    ;; difference is in the call to OBJECT-PRINT-LABEL for the symbol
    ;; package of an interned symbol - in lieu of OBJECT-PRINT-NAME in
    ;; the PRINT-NAME form of this method definition
    ;;
    ;; It may represents something of a PRINT-CANONC-SYMBOL macro, broadly.
    (multiple-value-bind (status pkg)
	(symbol-status object)
      (let ((keyword-p
	     (and pkg (eq pkg (find-package '#:keyword)))))
	(cond
	  (keyword-p
           ;; [NB]
	   (write-char #\: stream))
          ;; NB : may fall through to PRINT-LABEL for PKG
	  (pkg (princ (object-print-label pkg) stream))
	  (t (write-char #\# stream)
	     (write-char #\: stream)))
	(case status
	  (:external
	   (unless keyword-p ;; [NB]
	     (write-char #\: stream)))
	  ((:internal :inherited)
	   (write-char #\: stream)
	   (write-char #\: stream)))
	(write-string (symbol-name object) stream)))))

;; (with-output-to-string (*terminal-io*) (print-label (find-class 'string) t))
;; => "CL:STRING"


(defgeneric object-print-name (object)  ;; FIXME: rename to OBJECT-PRINT-NAME
  ;; FIXME: #I18N

  ;; FIXME: Generic function FTYPE declarations
  ;; => ftype (function (t) simple-string)

  (:documentation
   "Return a unique, human-readable name for OBJECT

See also: `object-print-label'")
  ;; FIXME: This updates some qualities of the dobelle-app source tree

  (:method (object)
    (with-output-to-string (s)
      (print-name object s)))
  (:method ((object string))
    (values object))
  (:method ((object package)) ;; NB: Simple package name form
    (package-name object))
  (:method ((object symbol))
    (with-output-to-string (s)
      (print-name object s)))
  (:method ((object class))
    (object-print-name (class-name object))))

;; (object-print-name (find-package '#:utils))
;; => "INFO.METACOMMUNITY.CLTL.UTILS"
;; (object-print-name '#:foo)
;; => "#:FOO"
;; (object-print-name 'print)
;; => "COMMON-LISP:PRINT"
;; (object-print-name ':test)
;; = ":TEST"

(defgeneric object-print-label (object) ;; FIXME: rename to OBJECT-PRINT-LABEL
  ;; FIXME: This updates some qualities of the dobelle-app source tree

  ;; FIXME: Generic function FTYPE declarations
  ;; => ftype (function (t) simple-string)
  (:documentation
   "Return a succinct, unique, human-readable label for OBJECT

See also: `object-print-name'")
  (:method (object)
    (princ-to-string object))
  (:method ((object string))
    (values object))
  (:method ((object package))
    ;; NB return the shortest string from the set of the package name
    ;; and any pacakge nicknames.
    ;;
    ;; In an estimate that that would typically comprise a short set,
    ;; this methodology produces some "Consing," for using a simple
    ;; "Sort a List" form, rather than developing any more per se
    ;; tedious iteration for computing the shortest string.
    (let ((name (package-name object))
	  (nicknames (package-nicknames object)))
      (car (sort (cons name (copy-list nicknames))
		 #'(lambda (a b)
		     (declare (type simple-string a b))
		     (< (length a) (length b)))))))
  (:method ((object symbol))
    (with-output-to-string (s)
      (print-label object s)))
  (:method ((object class))
    (object-print-label (class-name object))))

;; (object-print-label (find-package '#:utils))
;; => "UTILS"
;; (object-print-label '#:foo)
;; => "#:FOO"
;; (object-print-label 'print)
;; => "CL:PRINT"
;; (object-print-label ':test)
;; => ":TEST"
;; (object-print-label (find-class 'stream))
;; => "CL:STREAM"

(defclass pretty-printable-object ()
  ;; The 'label' and 'name' arguments might seem redundant.
  ;; This class was defined, originally, as a mixin for
  ;; application in the igneous-math system
  ((print-label
    :initarg :print-label
    :type simple-string
    :accessor object-print-label)
   (print-name
    :initarg :print-name
    :type simple-string
    :accessor object-print-name))
   (:documentation
    "Mixin class for OBJECT-PRINT-LABEL, OBJECT-PRINT-NAME functions.

See also:
* `format-label'
* `print-label'
* `print-name'"))


(defmethod print-name ((object pretty-printable-object) (stream stream))
  (prin1 (object-print-label object) stream))

(defmethod print-label ((object pretty-printable-object) (stream stream))
  (princ (object-print-label object) stream))



(defgeneric format-label (stream arg colon-p at-p)
  ;;; FIXME: Handle pprint-dipstach, etc
  (:documentation "Function for pretty-printing objects in format control strings
Example:

  (format nil \"Class ~/utils:format-label/\" (find-class 'string))
  => \"Class CL:STRING\"

")
  (:method (stream arg colon-p at-p)
    "If AT-P is NIL, PRINC to STREAM: the value of OBJECT-PRINT-NAME applied onto ARG

If AT-P is non-nil, PRINC to STREAM: the value of OBJECT-PRINT-LABEL applied onto ARG"
    (declare (ignore colon-p))
    (cond
      (at-p
       (princ (object-print-name arg) stream))
      (t
       (princ (object-print-label arg) stream)))))


;; (format nil "Class ~/utils:format-label/" (find-class 'string))
;; => "Class CL:STRING"

(defun print-hash-table (table
                         &key
			   (stream *standard-output*)
			   (format-control
			    (load-time-value
                             ;; NB: The NAMESPACE/SUBNAMESPACE naming
                             ;; convention would be problematic, here
			     (compile nil
				      (formatter "~%~/ltp.common:format-label/ : ~/ltp.common:format-label/")))))
  (declare (type hash-table table)
	   (type format-control format-control)
           (type stream-designator stream))
  (maphash (lambda (k v)
             (format stream format-control k v))
           table))

#-(and)
(eval-when ()
  (with-output-to-string (*standard-output*)
    (print-hash-table asdf/system-registry:*registered-systems*))
  )


(defgeneric object-name (object))
(defgeneric (setf object-name) (new-value object))
;; ^ NB: Previously defined as corresponding to ASSOCIATIVE-OBJECT
;;
;; See also ../mop/aclass/
