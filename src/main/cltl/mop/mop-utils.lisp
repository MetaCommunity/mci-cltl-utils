;; mop-utils.lisp - utilities onto MOP implementations
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

(in-package #:ltp-mop-utils)


(defmacro validate-class (class &optional (superclass 'standard-class))
  #+(or SBCL CMU CCL ALLEGRO)
  `(progn
     (defmethod validate-superclass ((a ,class) (b ,superclass))
       (values t))
     (defmethod validate-superclass ((a ,superclass) (b ,class))
       (values t)))
  #-(or SBCL CMU CCL ALLEGRO)
  `(progn
     (simple-style-warning
      "~<validate-class -~> ~<No known class/superclass validation ~
in this implementation -~> ~<class ~s with superclass ~s~>"
      (quote ,class) (quote ,superclass))
     (values)))
