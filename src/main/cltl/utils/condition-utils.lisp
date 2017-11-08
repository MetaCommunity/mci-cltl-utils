

(in-package #:mcicl.utils)

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
      (call-next-method)))
 
 (define-condition weather-error (error weather-condition)
   ())

 (define-condition rain-error (weather-error)
   ((weather
     :initform \"Rainy\")))

 (define-condition rainy-locomotion-error (rain-error)
   ((quality
     :initarg :quality
     :accessor ece-quality)
    (vehicle-type
     :initarg :vehicle-type 
     :accessor ece-vtype)
    (vehicle-applicaiton
     :initarg :vehicle-application
     :accessor ece-vapplication)))

 (defmethod format-condition :around ((c rainy-locomotion-error) 
                                      (s stream))
  (call-next-method)
  (terpri s)
  (format s \"Caution: ~A for ~A ~A\"
     (ece-quality c)
     (ece-vtype c)
     (ece-vapplication c)))

 (error 'rainy-locomotion-error
   :quality \"Unsafe\"
   :vehicle-type \"Formula 1\"
   :vehicle-application \"Racing\")")

  (:method ((condition condition) (stream symbol))
"Dispatch for FORMAT-CONDITION onto a symbolic output stream designator. 
This method allows for symbolic indication of an output stream to
FORMAT-CONDITION, with a syntax conformant onto ANSI CL (CLtL2)"
    (format-condition condition
                      (ecase stream
                        ((t) *terminal-io*)
                        ((nil) *standard-output*))))

  (:method ((condition simple-condition) (stream stream))
    "Apply the format control and format arguments for the CONDITION
onto the specified STREAM.

If a next method is defined in the effective method chain after this
method, then that next method will be evaluated before the
format-control and format-arguments for the CONDITION are applied to  
the stream. A TERPRI call will be evaluted after the next methods"
    (when (next-method-p)
      (call-next-method)
      (terpri stream))
    (format stream (simple-condition-format-control condition)
            (simple-condition-format-arguments condition)))
  (:method :after ((condition condition) (stream stream))
           "Ensure FINISH-OUTPUT is called onto STREAM"
           (finish-output stream)))

#+NIL ;; Test for print function
(error 'simple-condition :format-control "Random event ~S at ~S"
       :format-arguments (list (gensym "e-") 
                               (get-universal-time)))

(define-condition entity-condition ()
  ((name
    :initarg :name
    :reader entity-condition-name)))

(define-condition entity-not-found (error entity-condition)
  ()
  (:report format-condition))

(defmethod format-condition ((c entity-not-found) (s stream))
  (format s "Not found: ~S"
          (entity-condition-name c)))

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
  ((container
    :initarg :container
    :reader container-condition-container)))

