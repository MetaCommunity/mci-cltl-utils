;; reader-utils.lisp - reader utilities
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

(in-package #:ltp-utils)

;; NB: This assumes a Unicode manner of character encoding, and may not
;; be thoroughly portable as such.
;;
;; FIXME/NB: Up to and including NAME-CHAR-P the following definitions
;; represent something of an approach for developing a simple
;; character-oriented name parser such that may be intrinsically
;; interopable with name strings in XML syntax.

(deftype character-code ()
  '(integer 0 (#.char-code-limit)))

(defmacro code-in-range (c rmin rmax)
  `(<= (the character-code ,rmin)
       (the character-code ,c)
       (the character-code ,rmax)))


;; NB: The following forms are defined principally for operation onto
;; numeric character codes.
;;
;; Note also the CL-BABEL API for charset compat across implementations

(defmacro code= (c code)
  `(= (the character-code ,c)
      (the character-code ,code)))

(declaim (inline name-start-char-p
                 code-name-char-p char-name-char-p
                 #| read-name-string read-characters |#
                 ))

(defun code-name-start-char-p (c)
  ;; cf. http://www.w3.org/TR/xml/#NT-NameStartChar
  (declare (type character-code c)
	   (values boolean))
  (unless (code=  c #.(char-code #\Space ))
    (or
     (code-in-range c #.(char-code #\a) #.(char-code #\z))
     (code-in-range c #.(char-code #\A) #.(char-code #\Z))
     (code= c #.(char-code #\_))
     (code= c #.(char-code #\:)) ;; NB ":" as a name start char - valid for some XML, but inadvisable cf. [XMLNS]
     ;; see also : {other sys}:ncname-start-char-p
     (code-in-range c #xC0 #xD6)
     (code-in-range c #xD8 #xF6)
     (code-in-range c #xF8 #x2FF)
     (code-in-range c #x370 #x37D)
     (code-in-range c #x37F #x1FFF)
     (code-in-range c #x200C #x200D)
     (code-in-range c #x2070 #x218F)
     (code-in-range c #x2C00 #x2FEF)
     (code-in-range c #x3001 #xD7FF)
     (code-in-range c #xF900 #xFDCF)
     (code-in-range c #xFDF0 #xFFFD)
     (code-in-range c #x10000 #xEFFFF))))

(defun char-name-start-char-p (c)
  (declare (type character c)
           (values boolean))
  (code-name-start-char-p (char-code c)))


(defun code-name-char-p (c)
  ;; cf. http://www.w3.org/TR/xml/#NT-NameChar
  ;;
  ;; NB: This API has define separate CODE-NAME-CHAR-P and
  ;; CHAR-NAME-CHAR-P functions, to one effect of obviating an
  ;; "Unreachable Code" error  from SBCL when compiling READ-NAME-STRING
  ;; w/ this function inline.
  ;;
  ;; The principal effect has been of developing one specialized
  ;; function for each type of argument, C.
  (declare (type character-code c)
	   (values boolean)
	   (inline name-start-char-p))
  (unless (code=  c #.(char-code #\Space ))
    (or (code-name-start-char-p c)
	(code-in-range c #.(char-code #\0) #.(char-code #\9))
	(code= c #.(char-code #\-))
	(code= c #.(char-code #\.))
	(code= c #xB7)
	(code-in-range c #x0300 #x036F)
	(code-in-range c #x203F #x2040))))

;; (name-char-p #\A)
;; => T
;; (name-char-p #\.)
;; => T
;; (name-char-p #\Space)
;; => NIL


(defun char-name-char-p (c)
  (declare (type character c)
           (values boolean)
           (inline code-name-char-p))
  (code-name-char-p (char-code c)))



(defun read-name-string (s &optional (eof-error-p t) eof-value)
  (declare (type stream s)
	   (values t))
  (with-output-to-string (buff)
    (let ((n 0))
      (declare (type (integer 0 1) n))
      (loop
	 (let ((c (peek-char nil s nil)))
	   (cond
	     ((and c (char-name-char-p c))
	      (when (zerop n) (setq n 1)) ;; [?]
	      (write-char (read-char s) buff))
	     ((zerop n) ;; EOF
	      (cond
		(eof-error-p
		 (error 'end-of-file :stream s))
		(t (return-from read-name-string eof-value))))
	     (t (return))))))))

;; (with-input-from-string (s "kfoo 100 +") (read-name-string s ))
;; => "kfoo"

;; (with-input-from-string (s "kfoo") (read-name-string s ))
;; => "kfoo"

;; (with-input-from-string (s "") (read-name-string s ))
;; --> error (end-of-file)

;; (with-input-from-string (s "") (read-name-string s nil))
;; => NIL

;; (with-input-from-string (s "") (read-name-string s nil ':KEYWORD))
;; => :KEYWORD


(defun read-characters (stream n &key (element-type 'character)
				   (eof-error-p t)
				   eof-value)
  "Read N many characters from STREAM, returning a string of length N
and of the specified ELEMENT-TYPE as a sequence of those characters,
second value NIL, and third value, the value N

If end of file is encountered on the STREAM before N many characters
have been read:
 1. If EOF-ERROR-P is NON-NIL then an END-OF-FILE error is signaled
 2. Otherwise, if EOF-ERROR-P is NIL then EOF-VALUE is returned as the
    first value, with the second return value being the string as
    initalized until EOF, and as the third return value, the number of
    characters read from STREAM until EOF."
  (declare (type stream-designator stream)
	   (type array-dim n)
	   (type type-designator element-type)
	   (values t (or simple-string null)
		   array-dim))
  #+NIL ;; FIXME: Implement this as a compiler warning?
  (when (and eof-value (null eof-error-p))
    (simple-style-warning "EOF-VALUE specified with EOF-ERROR-P NIL: ~S"
			  eof-value))
  (let ((buff (make-array n :element-type element-type))
	(eof (load-time-value (gensym "EOF-") t)))
    (declare (type simple-string buff)
	     (type symbol eof))
    (dotimes (%n n (values buff nil n))
      (declare (type array-dim %n))
      (let ((c (read-char stream nil eof)))
	(cond
	  ((eq c eof)
	   (cond (eof-error-p
		  (error 'end-of-file :stream stream))
		 (t (return (values eof-value buff %n)))))
	  (t (setf (schar buff %n) c)))))))

;; (with-input-from-string (s "42FOO42") (read-characters s 2))
;; => "42", NIL, 2
;; (with-input-from-string (s "42FOO42") (read-characters s 42))
;; --> EOF-ERROR
;; (with-input-from-string (s "42FOO42") (read-characters s 8 :eof-error-p nil ))
;; => NIL, <string with null char>, 7
;; (with-input-from-string (s "42FOO42") (read-characters s 8 :eof-error-p nil :eof-value 42))
;; => 42, <string with null char>, 7
