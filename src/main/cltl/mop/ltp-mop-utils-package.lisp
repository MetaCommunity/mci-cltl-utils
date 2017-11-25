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

(defpackage #:ltp-mop-utils
  (:use  #:utils.ltp.thinkum.space
         #:c2mop
         #+LTP_PROTOTYPES #:bordeaux-threads
         #:cl
         )
  #+(or SBCL CMU CCL ALLEGRO) ;; FIXME: PCL-PORT
  (:shadowing-import-from
   #+SBCL #:SB-MOP
   #+CMU #:PCL
   #+CCL #:CCL
   #+ALLEGRO #:MOP
   #:validate-superclass
   #:standard-generic-function
   #:defmethod
   #:defgeneric
   #:standard-class
   )
  #+(or SBCL CMU CCL ALLEGRO) 
  ;; TO DO: Keep this synchronized onto PCL-PORT
  ;;
  ;; TBD: Why C2MOP shadows MOP implementations?
  ;;
  ;;     Possible issue: When shadowing a MOP implementaion, an
  ;;       application may also effectively be shadowing any compiler
  ;;       optimizaitons defined for the MOP implementation, such as
  ;;       may be defined in a manner specific to the individual MOP
  ;;       implementation.
  (:export
   #:validate-superclass ;; PCL
   #:standard-generic-function
   #:defmethod
   #:defgeneric
   #:standard-class
   )

  ;; TBD: Does the return value for the following form differ per
  ;; implementation? [A Review of C2MOP]
  ;;
  ;;   (package-shadowing-symbols (find-package '#:c2mop))
  
  (:export
   #:validate-class
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

(let* ((p (find-package '#:ltp-mop-utils))
       (s (package-shadowing-symbols p)))
  ;; NB: No package loxk held, during this operation
  (do-external-symbols (xs '#:c2mop)
    (unless (find (the symbol xs) (the list s) 
                  :test #'eq)
      (export s p))))
