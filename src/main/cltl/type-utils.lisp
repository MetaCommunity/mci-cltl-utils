
(in-package #:utils)

(deftype file-name ()
  `(or string pathname))

(deftype file-designator ()
  `(or string pathname file-stream))
