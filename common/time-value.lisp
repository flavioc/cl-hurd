
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

(defun make-time-value (&key (seconds -1) (microseconds -1))
  "Create a new time-value object."
  (declare (type fixnum seconds microseconds))
  (let ((ptr (foreign-alloc 'time-value-struct)))
    (setf (foreign-slot-value ptr 'time-value-struct 'seconds) seconds
          (foreign-slot-value ptr 'time-value-struct 'microseconds) microseconds)
    (let ((obj (make-instance 'time-value :ptr ptr)))
      (tg:finalize obj (lambda () (foreign-free ptr)))
      obj)))

(defmethod seconds ((time time-value))
  (let ((ret (foreign-slot-value (ptr time)
                                 'time-value-struct
                                 'seconds)))
    (if (= -1 ret)
      (maptime-seconds *mapped-time*)
      ret)))


(defmethod microseconds ((time time-value))
  (let ((ret (foreign-slot-value (ptr time)
                                 'time-value-struct
                                 'microseconds)))
    (if (= -1 ret)
      (maptime-microseconds *mapped-time*)
      ret)))

(defmethod time-value-eq ((time1 time-value) (time2 time-value))
  (and (= (seconds time1)
          (seconds time2))
       (= (microseconds time1)
          (microseconds time2))))

(defconstant +now-time-value+ (make-time-value))

(defmethod print-object ((time time-value) stream)
  (if (time-value-eq time +now-time-value+)
    (format stream "#<time-value NOW>")
    (format stream "#<time-value seconds=~s microseconds=~s"
            (seconds time)
            (microseconds time))))

(defmethod translate-from-foreign (value (type time-value-type))
  "Translate a time-value pointer to a time-value object."
  (if (= -1 (foreign-slot-value value 'time-value-struct
                                'microseconds))
    +now-time-value+
    (make-instance 'time-value :ptr value)))

(defmethod translate-to-foreign (value (type time-value-type))
  "Translate a time-value object to a foreign time-value pointer."
  (ptr value))

