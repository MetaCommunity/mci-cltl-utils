;; mop-pkg.lisp - package definition for info.metacommunity.cltl.utils.mop

(defpackage #:info.metacommunity.cltl.utils.mop
  (:nicknames #:mcicl.mop)
  (:use  #:info.metacommunity.cltl.utils
         #:c2mop
         #:bordeaux-threads
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
   ))
