;; mop-pkg.lisp - package definition for info.metacommunity.cltl.utils.mop
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2017 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial Implementation
;;
;;------------------------------------------------------------------------------

(defpackage #:ltp/common/mop
  (:nicknames #:ltp.common.mop)
  (:use  #:ltp/common
         #:c2mop
         #+LTP_PROTOTYPES #:bordeaux-threads
         #:cl
         )
  #+(or SBCL CMU CCL ALLEGRO) ;; NB: PCL
  (:shadowing-import-from
   #+SBCL #:SB-MOP
   #+CMU #:PCL
   #+(or MCL OPENMCL) #:CCL
   #+ALLEGRO #:MOP
   #:validate-superclass
   #:standard-generic-function
   #:defmethod
   #:defgeneric
   #:standard-class
   )
  #+(or SBCL CMU CCL ALLEGRO)
  ;; NB: C2MOP shadows MOP implementations.
  ;;
  ;;     Possible issue: When shadowing a MOP implementaion, an
  ;;     application may also effectively be shadowing any compiler
  ;;     optimizaitons, such as may be defined in a manner specific to
  ;;     the individual MOP implementation.
  ;;
  ;;
  ;;     In CMUCL, SBCL, CCl, and Allegro Lisp implementations, this
  ;;     system will endeavor to un-shadow the underyling MOP
  ;;     implementation.
  ;;
  (:export
   #:validate-superclass ;; PCL
   #:standard-generic-function
   #:defmethod
   #:defgeneric
   #:standard-class
   )


  (:export
   #:validate-class
   )

  ;; TBD: Does the return value for the following form differ per
  ;; implementation? [A Review of C2MOP]
  ;;
  ;;   (package-shadowing-symbols (find-package '#:c2mop))

  #+LTP_PROTOTYPES
  (:export
   #:associative-index
   #:associative-table-index
   #:object-table-lock
   #:object-table
   #:simple-associative-index
   #:object-table-key-function
   #:compute-key
   #:register-object
   #:find-object
   #:remove-object
   #:map-objects
   #:associative-class
   #:simple-associative-class
   #:object-key-slot
   )


  )


;; Make C2MOP symbols avaialble from this package,
;; except for those shadowed by this package.

(let* ((p (find-package '#:ltp/common/mop))
       (s (package-shadowing-symbols p)))
  ;; NB: No package lock held, during this operation
  (do-external-symbols (xs '#:c2mop)
    (unless (find (the symbol xs) (the list s)
                  :test #'eq)
      (export s p))))
