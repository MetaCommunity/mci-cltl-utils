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


(in-package #:ltp/common)


(defpackage #:ltp/common/mop
  (:nicknames #:ltp.common.mop)
  (:use  #:ltp/common
         #:closer-mop
         #:cl)
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
  ;; un-shadow symbols shadowed by C2MOP
  (:export
   #:validate-superclass ;; NB PCL
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

(let* ((p-dst (find-package '#:ltp/common/mop))
       (p-org (find-package '#:closer-mop))
       (s (package-shadowing-symbols p-org)))
  (declare (type cons s))
  (labels ((not-shadowed-p (xs)
             (declare (type symbol xs))
             (not (find (the simple-string (symbol-name xs)) s
                        :key #'symbol-name
                        :test #'string=))))
    (let ((export
           (package-exports-symbols-if p-org #'not-shadowed-p)))
      (declare (type cons export))
      (export export p-dst)
      (values export))))
