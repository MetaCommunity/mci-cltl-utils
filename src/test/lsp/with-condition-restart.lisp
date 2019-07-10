;; with-condition-restart.lisp -- portable condition/restart integration
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

(in-package #:ltp/stor/fdef)

;; NB: The following definitions should be available within the Common
;; Lisp package #:ltp/stor/fdef - whether defined there, defined in any
;; system in use by the same.
;;
;; - FINALIZED-CLASS (Type)
;; - UNFINALIZED-CLASS (Condition Type)
;;
;; These definitions are required for evaluation of the source forms, below
;;
;; These definitions may be available via the source system
;; ltp-main:src/main/lsp/stor/ltp-stor-fdef.asd

;; --------------------

#+TBD
(defmacro with-conditional-restarts ((&rest restart-decls)
                                        form &rest handler-decls)
  )



;; NB: The following prototype form for ENSURE-CLASS-FINALIZED-P was
;; developed with regards to a short number of concerns:
;;
;; - as with regards to how the FINALIZE restart, provided within the
;;   function's definition, may be invoked from the debugger
;;
;; - principally, as with regards to how a restart invoked due to an
;;   error condition may access the condition for which the restart was
;;   invoked. For interactive purposes, this is accopmlished with a
;;   lexically scoped function, below. This approach presents some
;;   limitations, however -- such as with regards to how that lexically
;;   scoped function is applied for the interactive form of the
;;   restart, as below, moreover as with regards to the lexical
;;   availabilty of a corresponding "Setter Form," such that -- in a
;;   senese -- must be  used within a corresponding function defined
;;   with HANDLER-BIND, to make the condition available to the restart,
;;   as with the following methodology.

(defun* %ensure-class-finalized-p (class)
  ;;
  ;; NB: This has been approached otherwise, in the initial definition
  ;; of LTP/STOR/FDEF::ENSURE-CLASS-FINALIZED-P -- in which, an assumption
  ;; corresponding to that function's present definition has been
  ;; denoted, in a precautionary manner, in that function's source form.
  ;;
  ;; This definition is provided as a prototype form, and may be later
  ;; adopted in redefinition of LTP/STOR/FDEF::ENSURE-CLASS-FINALIZED-P
  ;;
  (declare (type class-designator class)
           (values class &optional))
  (let ((%class (compute-class class))
        (*condition*))
    (declare (type class %class)
             (type (or null condition) *condition)
             (special *condition*))
    (labels (#+NIL (set-conndition (c)
                     ;; refer to remarks in the HANDLER-BIND source
                     ;; form, below. This lexically scoped function may
                     ;; not be available for the condition handler
                     ;; function, due to limitations with regards to
                     ;; environments in this program's evaluation.
                     (setq *condition* c))
             (get-condition-for-restart ()
               ;; FIXME: Not in itself an interactive I/O form, this
               ;; function simply serves to facilitate exact matching of
               ;; (A) a restart to (B) a condition for which the restart
               ;; was invoked. As such, it's used in lieu of the
               ;; :INTERACTIVE form for the FINALIZE restart, below.
               ;;
               ;; This is, in a sense, a "Hack" provided here for
               ;; purpose of illustrating the semantics of this
               ;; programming pattern.
               ;;
               ;; Considering that there must be a manner of logical
               ;; distiniction betweeh (A) HANDLER-BIND and RESTART-BIND
               ;; forms, in a program, (B) source forms such that may
               ;; interact, whatsoever directly or indirectly, with the
               ;; forms per [A],  and (C) the call forms representing
               ;; the interactive debugger, in any implementation
               ;; environment:  Perhaps there may be any other approach
               ;; possible, logically, towards addressing this concern
               ;; with regards to accessing the condition for which a
               ;; restart would be invoked, in the semantic
               ;; specificatoin of anysingle restart --  as whether
               ;; invoked interactively from within the debugger or
               ;; invoked directly in a program call forms.
               ;;
               ;; This source form presents, simply, a prototype in a manner
               ;; of a generalized programming pattern, illustrated
               ;; about a specific usage case, namely: To check whether
               ;; a class is finalized and, if it is not, to allow the
               ;; user to interactively specificy, or to allow any
               ;; calling program to non-interactively specify that the
               ;; class should be finalized per default. This, in
               ;; itself, may not represent a very broad concern for
               ;; application environments except insofar as an
               ;; application may require that a class is finalized
               ;; before any subsequent program forms
               ;;
               ;; This prototype, in itself, was produced pursuant
               ;; towards runtime computation of FTYPE declarations for
               ;; functional accessors onto slots of finalized
               ;; classes.
               ;;
               ;; In a sense with regards to expected usage cases, this
               ;; may seem to represents a manner of"IDE Sugar."
               ;;
               ;; Ideally, the UNFINALIZED-CLASS condition should not
               ;; ever be reached in any user installed program. Some
               ;; concerns as may be entailed of an UNFINALIZED-CLASS
               ;; condition may not be addressed without regards to the
               ;; definition of the class for which the condition itself
               ;; would be signaled.
               ;;
               (list *condition*)))
    (handler-bind ((unfinalized-class (lambda (c)
                                        ;; NB: DNW - cf. lexical
                                        ;; environments in Common Lisp
                                        #+DNW (set-condition c)
                                        (setq *condition* c)
                                        )))
      (restart-case
          (progn
            (or (class-finalized-p %class)
                (error 'unfinalized-class
                       :expecteted-type 'finalized-class
                       :datum %class))
            (values %class))
        ;; TBD: RESTART-CASE like HANDLER-BIND (??)
        (finalize (&optional c)
          :report "Finalize Class"
          ;; NB: It may seem that the following should be approached in
          ;; something of a more prototypical manner -- simply to ensure
          ;; that a condition for which a restart is invoked will be
          ;; available to the restart's restart function, when that
          ;; function is invoked, even when that restat is invoked
          ;; interactively within a debugger.
          ;;
          ;; This assumes that when this restart is invoked
          ;; interactively, a HANDLER-BIND call -- as per the condition
          ;; type of the condition to be handled, here -- will have set
          ;; the condition as the binding of the *CONDITION*
          ;; varaible. Failing that external call, moreover during
          ;; an interactive invocation of this restart, the identity of
          ;; the condition for which this FINALIZE restart is invoked
          ;; may not be unambiguously determined here.
          ;;
          :interactive get-condition-for-restart
          (typecase c
            (unfinalized-class
             ;; NB: DNW if the calling procedure is not providing the
             ;; condition to this restart.
             ;;
             ;; Towards a portable definition of this restart, if the
             ;; restart is reached via any manner external to this
             ;; function -- vis a vis Common Lisp #'CONTINUE -- it
             ;; should only be invoked for a suitable condition such
             ;; that would be provided to the restat, when the restart
             ;; is invoked. When invoked interactively, the condition
             ;; may be accessed -- such as here -- from within the
             ;; calling lexical environment.  When invoked directly from
             ;; a program procedure, the condition should be provided to
             ;; that procedure -- i.e the analogous FINALIZE restart
             ;; function would require a single argument, denoting a
             ;; condition for which it is being invoked, rather than
             ;; only providing an optional argument such as with
             ;; CL:CONTINUE
             ;;
             ;; Although this may seem like an awkward manner of
             ;; indirection with regars to the identity of the class to
             ;; be finalized, it should serve to ensure appropriate
             ;; matching of the restart to the respective condition -
             ;; not supporting arbitrary finalization of other classes,
             ;; per se.
             ;;
             ;; NB: For how this restart is defined in a manner
             ;; principally local to this function's source form, it may
             ;; not be logically possible for this restart to be invoked
             ;; other than as a result of the failed CLASS-FINALIZED-P
             ;; test, above -- unless, per se, any form within the
             ;; evaluation of that CLASS-FINALIZED-P call would, in
             ;; itself, endeavor to call to this restart function,
             ;; assuming that this restart binding would be accessible
             ;; in that other function's effective lexical environment.
             ;;
             ;; As such, this source form in itself may serve to provide
             ;; an example -- in a sense, a program source template --
             ;; towards a semantics for actuation of restarts
             ;; within the debugger environment -- vis a vis, the
             ;; arguments provided to a restart, when selected
             ;; interactively within the debugger or when invoked from
             ;; within a program, namely for purpose of handling a
             ;; single error of a known condition type, moreover as
             ;; with regards to the lexical environment in which the
             ;; restart is invoked. This is, in a practical sense,
             ;; intended for purpose of ensuring a manner of logical
             ;; consistency in program call forms.

             ;; NB: Besides accessing the condition for which the
             ;; restart is invoked, this form also acceses the identity
             ;; of the class for which the initial %ENSURE-CLASS-FINALIZED-P
             ;; call was invoked - namely, as accessed via the lexical
             ;; environment in which the restart's effective restart
             ;; function is assumed to be defined.
             (let ((%%class (type-error-datum c)))
               (cond
                 ((eq %%class %class)
                  (finalize-inheritance %class)
                  (values %class))
                 (t ;; NB: "Odd Catch," but this may be "Logically Reachable"
                  (simple-program-error
                   "Class ~S is not EQ to ~S"
                   %%class %class)))))
            ;; NB: "Odd Catch," for when C is a condition not created per
            ;; the ASSERT call, above - Dispatch per type of C
            (error (error c))
            (warning (warning c))
            (condition (signal c))
            (null (simple-program-error
                   "No condition provided to FINALIZE restart"))
            (t
             (simple-program-error "~<Unknown condition ~S~>~
~< provided to FINALIZE restart~>"
                                   c)))))))))

;; (%ensure-class-finalized-p 'string)
;; (%ensure-class-finalized-p (defclass #.(make-symbol "frob") () ()))

;; --------------------

;; TBD: Declaration of *CONDITION* as a global variable; Portable
;; definition of macro forms and functional forms, for storing and
;; accessing *CONDITION* from within HANDLER-BIND/HANDLER-CASE and
;; corresponding RESTART-BIND/RESTART-CASE forms, in a manner as
;; illustrated in the previous.
