;; codition-utils.lisp
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


(in-package #:ltp-utils)

;; NOTE analogous definitions in CMUCL, SBCL
;; NOTE portability (is not privatization)

(define-condition simple-style-warning (style-warning simple-condition)
  ())

(defmacro simple-style-warning (fmt-ctrl &rest args)
  `(warn 'simple-style-warning
	 :format-control ,fmt-ctrl
	 :format-arguments (list ,@args)))


(define-condition simple-program-error (program-error simple-condition)
  ())

(defmacro simple-program-error (fmt-ctrl &rest args)
  `(error 'simple-program-error
	  :format-control ,fmt-ctrl
	  :format-arguments (list ,@args)))


(defgeneric format-condition (condition stream)
  (:documentation
   "CLOS Utility for application within condition class REPORT forms

Example:

 (define-condition weather-condition ()
    ((weather
       :initarg :weather
       :accessor weather-condition-weather))
    (:report format-condition))

 (defmethod format-condition ((c weather-condition) (s stream))
   (format s \"The weather is ~S\"
      (weather-condition-weather c))
   (when (next-method-p)
      (terpri s)
      (call-next-method)))")

  (:method ((condition condition) (stream symbol))
    ;; FIXME_DESIGN symbol as stream designator => constant
"Dispatch for FORMAT-CONDITION onto a symbolic output stream designator.
This method allows for symbolic indication of an output stream to
FORMAT-CONDITION, with a syntax compatible onto ANSI CL (CLtL2)"
;; FIXME_DOCS See also: CL:PRINT; LTP `string-designator' type; LTP
;; `compute-output-stream'
    (format-condition condition
                      (ecase stream
                        ((t) *terminal-io*)
                        ((nil) *standard-output*))))

  (:method ((condition simple-condition) (stream stream))
    "Apply the format control and format arguments for the CONDITION,
with output to the specified STREAM.

If a next method is defined in the effective method sequence after this
method, then that next method will be evaluated before the
format-control and format-arguments for the CONDITION are applied for
output to the stream. WA TERPRI call will be evaluted after the next
method has returned, and before the direct format procedure in this method"
    ;; FIXME_DOCS describe, illustrate the rationale for the method
    ;; dispatching behavior denoted in the docstring - e.g albeit
    ;; trivially, "It's not a frame stack," vis a vis serial backtrace
    ;; output
    (when (next-method-p)
      (call-next-method)
      (terpri stream))
    (apply #'format (the stream stream)
           (simple-condition-format-control condition)
           (simple-condition-format-arguments condition)))
  (:method :after ((condition condition) (stream stream))
           "Ensure FINISH-OUTPUT is called onto STREAM"
           (finish-output stream)))

#+NIL ;; Test for print function
(error 'simple-condition :format-control "Random event ~S at ~S"
       :format-arguments (list (gensym "e-")
                               (get-universal-time)))

;; -

(define-condition entity-condition ()
  ;; FIXME_DOCS note that this is not `cell-error'
  ((name
    :initarg :name
    :reader entity-condition-name)))

(define-condition entity-not-found (error entity-condition)
  ()
  (:report format-condition))

(defmethod format-condition ((c entity-not-found) (s stream))
  (format s "Not found: ~S"
          (entity-condition-name c)))

;; -

(define-condition redefinition-condition (style-warning)
  ((previous-object
    :initarg :previous
    :accessor redefinition-condition-previous-object)
   (new-object
    :initarg :new
    :accessor redefinition-condition-new-object))
  (:report format-condition))


 (defmethod format-condition ((c redefinition-condition) (s stream))
  (format s "Redefinition ~<~S~> => ~<~S~>"
          (redefinition-condition-previous-object c)
          (redefinition-condition-new-object c)))

(define-condition container-condition ()
  ;; FIXME_DOCS "Examples"
  ((container
    :initarg :container
    :reader container-condition-container)))

