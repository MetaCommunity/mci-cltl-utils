;; reader-utils.lisp - reader utilities

(in-package #:utils)

(deftype character-code ()
  '(integer 0 (#.char-code-limit)))

(defmacro code-in-range (c rmin rmax)
  `(<= (the character-code ,rmin)
       (the character-code ,c)
       (the character-code ,rmax)))


(defmacro code= (c code)
  `(= (the character-code ,c)
      (the character-code ,code)))

(defun name-start-char-p (c)
  ;; cf. http://www.w3.org/TR/xml/#NT-NameStartChar
  (declare (type (or character character-code) c)
	   (values boolean))
  (let ((cc (etypecase c
	      (character (char-code c))
	      (character-code c))))
    (declare (type character-code cc))
    (unless (code=  cc #.(char-code #\Space ))
      (or
       (code-in-range cc #.(char-code #\a) #.(char-code #\z))
       (code-in-range cc #.(char-code #\A) #.(char-code #\Z))
       (code= cc #.(char-code #\_))
       (code= cc #.(char-code #\:))
       (code-in-range cc #xC0 #xD6)
       (code-in-range cc #xD8 #xF6)
       (code-in-range cc #xF8 #x2FF)
       (code-in-range cc #x370 #x37D)  
       (code-in-range cc #x37F #x1FFF)
       (code-in-range cc #x200C #x200D)  
       (code-in-range cc #x2070 #x218F)
       (code-in-range cc #x2C00 #x2FEF)  
       (code-in-range cc #x3001 #xD7FF)
       (code-in-range cc #xF900 #xFDCF) 
       (code-in-range cc #xFDF0 #xFFFD)
       (code-in-range cc #x10000 #xEFFFF)))))


(defun name-char-p (c)
  ;; cf. http://www.w3.org/TR/xml/#NT-NameChar
  (declare (type character c)
	   (values boolean)
	   (inline name-start-char-p))
  (let ((cc (etypecase c
	      (character (char-code c))
	      (character-code c))))
    (declare (type character-code cc))
    (unless (code=  cc #.(char-code #\Space ))
      (or (name-start-char-p cc)
	  (code-in-range cc #.(char-code #\0) #.(char-code #\9))
	  (code= cc #.(char-code #\-))
	  (code= cc #.(char-code #\.))
	  (code= cc #xB7)
	  (code-in-range cc #x0300 #x036F) 
	  (code-in-range cc #x203F #x2040)))))

;; (name-char-p #\A)
;; => T
;; (name-char-p #\.)
;; => T
;; (name-char-p #\Space)
;; => NIL

(defun read-name-string (s &optional (eof-error-p t) eof-value)
  (declare (type stream s)
	   (values t))
  (with-output-to-string (buff)
    (let ((n 0))
      (declare (type (integer 0 1) n))
      (loop
	 (let ((c (peek-char nil s nil)))
	   (cond
	     ((and c (name-char-p c))
	      (when (zerop n) (setq n 1))
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

