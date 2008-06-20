
(define-foreign-type ports-class-type ()
					 ()
					 (:actual-type :pointer)
					 (:simple-parser ports-class-t))

(defclass ports-class ()
  ((pointer :initform (error "You must provide a foreign pointer")
			:initarg :pointer
			:accessor pointer)))

(defmethod translate-from-foreign (value (type ports-class-type))
  (make-instance 'ports-class :pointer value))

(defmethod translate-to-foreign (value (type ports-class-type))
  (pointer value))

(define-foreign-type ports-bucket-type ()
					 ()
					 (:actual-type :pointer)
					 (:simple-parser ports-bucket-t))

(defclass ports-bucket ()
  ((pointer :initform (error "You must provide a foreign pointer with :pointer")
   			:initarg :pointer
			:accessor pointer)))

(defmethod translate-from-foreign (value (type ports-bucket-type))
  (make-instance 'ports-bucket :pointer value))

(defmethod translate-to-foreign (value (type ports-bucket-type))
  (pointer value))

(define-foreign-type port-info-type ()
					 ()
					 (:actual-type :pointer)
					 (:simple-parser port-info-t))

