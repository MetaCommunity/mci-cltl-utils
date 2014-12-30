;; mop-pkg.lisp - package definition for info.metacommunity.cltl.utils.mop

(defpackage #:info.metacommunity.cltl.utils.mop
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
   )
  #+SBCL
  (:shadowing-import-from
   ;; prefer implementation's own forms to those defined in C2MOP
   #:cl
   #:standard-generic-function
   #:defmethod
   #:defgeneric
   )
  (:export
   #:validate-class
   ))
