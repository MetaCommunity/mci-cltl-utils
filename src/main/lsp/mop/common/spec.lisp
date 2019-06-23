;; spec.lisp - local prototypes for method specialization onto MOP
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------


(in-package #:ltp/common/mop)

;; rcs -i -U spec.lisp
;; ci spec.lisp
;;
;; co spec.lisp

#+NIL ;; unused macro, in this revision
(defmacro do-cons ((first rest whence &optional return) &body body)
  (with-symbols (dispatch %whence)
    `(block nil
       (labels ((,dispatch (,%whence)
                  ;; NB: RETURN is referenced twice, below.
                  ;;
                  ;; The expanded value of RETURN will only be evaluated
                  ;; once.
                  (cond
                    ((consp ,%whence)
                     (let ((,first (car ,%whence))
                           (,rest (cdr ,%whence)))
                       ,@body
                       (cond
                         ((consp ,rest) (,dispatch ,rest))
                         (t (return ,return)))))
                    ((null ,%whence) (return ,return))
                    (t (error 'type-error
                              :expected-type 'list
                              :datum ,%whence
                              #+SBCL :context #+SBCL "as provided to DO-CONS"))
                    )))
         (,dispatch ,whence)))))

(eval-when ()

  (do-cons (a rest (class-precedence-list (find-class 'string))
              (values nil nil))
    (format t "~%~S" a))


  (do-cons (a rest '(a b . c)
              (values nil nil))
    (format t "~%~S : ~S" a rest))

  (do-cons (a b '(a . b) (values nil nil))
    (format t "~%~S : ~S" a b))

  (do-cons (a b '(a) (values nil nil))
    (format t "~%~S : ~S" a b))

  (do-cons (a b nil (values nil nil))
    (format t "~%~S : ~S" a b))


  (do-cons (a b 'arbitrary (values nil nil))
    (format t "~%~S : ~S" a b))


)

;; --------------------

(defmacro while* ((clause &optional retv) &body body)
  `(loop (or ,clause (return ,retv))
      (progn ,@body)))

;; (let ((n 10)) (while* ((not (zerop n)) n) (decf n)))

;; (let ((n 10)) (while* ((not (zerop n)) n) (when (= n 5) (return -10)) (decf n)))



(defmacro while (clause &body body)
  `(while* (,clause) ,@body))


;; (let ((n 10)) (while (not (zerop n)) (decf n)) n)


;; --------------------


#+NIL ;; unused macro, in this revision
(defmacro push-nth (n val whence)
  (with-symbols (%n %whence nc)
    `(let ((,%n ,n)
           (,%whence ,whence))
       (declare (type (integer 0) ,%n))
       (cond
         ((zerop ,%n)
          (setf ,whence (cons ,val ,%whence)))
         (t
          (let ((,nc (nthcdr (1- ,%n) ,%whence)))
            (cond
              ((consp ,nc)
               (push ,val (cdr ,nc))
               ,%whence)
              (t (error
                  "~<Cannot PUSH-NTH for index ~D of ~S :~>~
~< NTH CDR ~D => ~S is not a CONS~>"
                  ,%n ,%whence (1- ,%n) ,nc)))))))))

;; (let ((v (list 'a 'b))) (push-nth 0 t v) v)

;; (setq *print-circle* t)

;; (let ((v (list 'a 'b))) (push-nth 1 t v) v)

;; (let ((v (list 'a 'b))) (push-nth 2 t v) v)

;; FAIL NB:
;; (let ((v (list 'a 'b))) (push-nth 3 t v) v)



;; --------------------

;; Trivial prototyping for specialized method dispatching
;; with minimal consing in generic function calls


(defun mk-expandable-vec ()
  (make-array 0 :fill-pointer t :adjustable t))

#+NIL ;; unused here
(defun reset-expandable-vec (which)
  (setf (fill-pointer which) 0))



(eval-when ()

(let ((specialization-table (mk-expandable-vec))
      ;; ^ NB: The call forms actually coerce this to a list

      #+TD (call-table (mk-expandable-vec))
      )
  (labels ((mk-specialization-row (args)
             ;; create and store an analogy to a method specialization
             (vector-push-extend (coerce (mapcar #'compute-class  args)
                                             'simple-vector)
                                 specialization-table))


           (compute-call-info-for-n (cls specializer-offset known)
             ;; compute a list of all specializations for parameter of
             ;; class CLS at OFFSET in the specializable parameter list,
             ;; for an initial set of KNOWN specializations

             (let (;;; (start 0)
                   (depth 0)
                   (%known)
                   tmp-1 tmp-2
                   )


               #+DEBUG (warn "OK ~S KNOWN ~S" specializer-offset known)

               (dolist (c (class-precedence-list cls))
                 (let ((known known))
                   (while (progn #+DEBUG (warn "ITERATE ~D ~D ~D"
                                               specializer-offset depth (length known))
                                 known)
                     ;; (warn "ITERATE w/ KNOWN ~S" known)
                     (let ((n (position c (the cons known)
                                        :key #'(lambda (row)
                                                 (svref (cdr row) specializer-offset))
                                        :test #'eq
                                        ;; :test #'subtypep
                                      ;;; :start start
                                        )))
                       (cond
                         (n
                          #+DEBUG
                          (warn "~<GOT ~S for depth ~S of arg nr. ~S : ~>~< ~S~>"
                                n depth
                                specializer-offset
                                (nth n known))
                          (setq tmp-1 (nthcdr n known)
                                tmp-2 (car tmp-1))
                          (setf (car tmp-2) (nconc (car tmp-2) (list depth)))
                          (push tmp-2 %known)
                          (setq ;;; start n
                           known (cdr tmp-1)
                           ))
                         ;; else return from while
                         (t ;; (setq known nil)
                          (return))))) ;; WHILE
                   )
                 (incf depth)) ;; DOLIST

               #-NIL (values %known)
               #+NIL
               (sort %known #'(lambda (inst-a inst-b)
                                ;; generic composite sort-by-maximal-precedence
                                ;; for a generic function of three
                                ;; specializable params (May not be 110% MOP)
                                (destructuring-bind (a-n-1 &optional a-n-2 a-n-3)
                                    (car inst-a)
                                  (destructuring-bind (b-n-1 &optional b-n-2 b-n-3)
                                      (car inst-b)
                                    ;; FIXME no type optimization here.
                                    ;; When non-nil, each of these is an
                                    ;; unsigned fixnum
                                    (cond
                                      ((and a-n-3
                                            (= a-n-1 b-n-1)
                                            (= a-n-2 b-n-2))
                                       ;; NB This is the only case that
                                       ;; matters - call this from an
                                       ;; upper call, where A-N-3 and
                                       ;; A-N-2 := always avl
                                       (< a-n-3 b-n-3))

                                      ((and a-n-2
                                            (= a-n-1 b-n-1))
                                       (< a-n-2 b-n-2))
                                      (t (< a-n-1 b-n-1))
                                      )))))
               ))


           (compute-call-info-for (a b c)
             ;; compute for each arg
             ;; NB: this labels function can be computed with a template
             (let ((reachable  (mapcar #'(lambda (spec)
                                           (cons nil spec))
                                       (coerce specialization-table 'list))))

               (dolist (hack (list (list a 0 reachable) (list b 1) (list c 2))
                        #+NIL (values reachable))
                 ;; FIXME - Cheap convenience hack for iterative computation
                 ;; onto a static form. Uses needless CONS initialization
                 (destructuring-bind (param-class param-offset
                                                  &optional (%reachable reachable)) hack
                   #+DEBUG
                   (warn "Call for ~D with ~D reachable"
                         param-offset (length reachable))

                   (setq reachable
                         (compute-call-info-for-n param-class param-offset %reachable))
                   #+DEBUG
                   (warn "Call for ~D got reachable ~S" param-offset reachable)
                   ))

               (sort reachable #'(lambda (inst-a inst-b)
                                ;; generic composite sort-by-maximal-precedence
                                ;; for a generic function of three
                                ;; specializable params (May not be 110% MOP)
                                (destructuring-bind (a-n-1 a-n-2 a-n-3)
                                    (car inst-a)
                                  (destructuring-bind (b-n-1 b-n-2 b-n-3)
                                      (car inst-b)
                                    (declare (type (integer 0 #.most-positive-fixnum)
                                                   a-n-1 a-n-2 a-n-3
                                                   b-n-1 b-n-2 b-n-3)
                                             #-DEBUG
                                             ;; only if these depth values are
                                             ;; not referenced elsewhere
                                             (dynamic-precedence
                                              a-n-1 a-n-2 a-n-3
                                              b-n-1 b-n-2 b-n-3)
                                             )
                                    (cond
                                      ((and (= a-n-1 b-n-1)
                                            (= a-n-2 b-n-2))
                                       (< a-n-3 b-n-3))
                                      ((= a-n-1 b-n-1)
                                       (< a-n-2 b-n-2))
                                      (t
                                       (< a-n-1 b-n-1))
                                      )))))


               ))

           (compute-call-for (a b c)
             (compute-call-info-for (class-of a) (class-of b) (class-of c)))
           )


    (mk-specialization-row '(array fixnum t))
    (mk-specialization-row '(string integer t))
    (mk-specialization-row '(string fixnum t))
    (mk-specialization-row '(array integer t))
    (mk-specialization-row '(t fixnum t))
    (mk-specialization-row '(t fixnum cons))
    (mk-specialization-row '(t integer t))
    (mk-specialization-row '(t t symbol))
    (mk-specialization-row '(array t t))
    (mk-specialization-row '(string t t))
    (mk-specialization-row '(t t t))
    (mk-specialization-row '(t t list))

    (mk-specialization-row '(integer t t))
    (mk-specialization-row '(fixnum t t))


    (list (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                  ;; should be the entire set, sorted
                  (compute-call-for "Frob" 5 nil))

          (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                   ;; should be a sorted complete subset
                  (compute-call-for #() 5 nil))


          (mapcar #'(lambda (row)
                      (cons (car row) (mapcar #'class-name (coerce (cdr row) 'list))))
                   ;; should be a sorted complete subset
                  (compute-call-for 5 5 '(a b c)))

          )

    ;; (setq *print-circle* nil)

    ;; (pushnew :debug *features* :test #'eq)

    ))

;; =>
;; ((3 0 4) STRING FIXNUM T)
;; ((3 1 4) STRING INTEGER T)
;; ((3 5 4) STRING T T)
;; ((6 0 4) ARRAY FIXNUM T)
;; ((6 1 4) ARRAY INTEGER T)
;; ((6 5 4) ARRAY T T)
;; ((8 0 4) T FIXNUM T)
;; ((8 1 4) T INTEGER T)
;; ((8 5 1) T T SYMBOL)
;; ((8 5 2) T T LIST)
;; ((8 5 4) T T T)
;; , ...

)
