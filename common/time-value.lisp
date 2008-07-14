
(in-package :hurd-common)

(defcstruct time-value-struct
  "Time value struct returned by the Mach kernel.
Definition can be found at mach/time_value.h"
  (seconds :int)
  (microseconds :int))

(define-foreign-type time-value-type ()
  ()
  (:documentation "CFFI type for thye time-value-struct.")
  (:actual-type :pointer)
  (:simple-parser time-value-t))

(defclass time-value ()
  ((ptr :reader ptr
        :initarg :ptr
        :documentation "Pointer to a time-value struct."))
  (:documentation "Time-value class for objects that allocate pointers to time-value structures."))

(defmethod translate-from-foreign (value (type time-value-type))
  "Translate a time-value pointer to a time-value object."
  (if (= -1 (foreign-slot-value value 'time-value-struct 'microseconds))
    :now
    (make-instance 'time-value :ptr value)))

(defmethod translate-to-foreign (value (type time-value-type))
  "Translate a time-value object to a foreign time-value pointer."
  (ptr value))
