
(in-package :hurd-common)

;;
;; This implements a time-value class.
;; Objects of this class should be used to represent time,
;; namely the *time stat fields.
;;

(defcstruct time-value-struct
  "Time value struct returned by the Mach kernel.
Definition can be found at mach/time_value.h"
  (seconds :int)
  (microseconds :int))

(defclass time-value ()
  ((ptr :accessor ptr
        :initform nil
        :initarg :ptr
        :documentation "Pointer to a time-value struct.")
   (seconds :initarg :seconds
            :accessor seconds
            :initform nil
            :documentation "Number of seconds.")
   (microseconds :initarg :microseconds
                 :accessor microseconds
                 :initform nil
                 :documentation "Number of miliseconds."))
  (:documentation "Time-value class for objects that allocate pointers to time-value structures."))

(defun make-time-value (&key (seconds -1) (microseconds -1))
  "Create a new time-value object."
  (declare (type fixnum seconds microseconds))
  (make-instance 'time-value
                 :seconds seconds
                 :microseconds microseconds))

(defconstant +now-time-value+ (make-time-value) "Current time value.")

(define-foreign-type time-value-type ()
  ()
  (:documentation "CFFI type for thye time-value-struct.")
  (:actual-type :pointer)
  (:simple-parser time-value-t))

(defmethod translate-from-foreign (value (type time-value-type))
  "Translate a time-value pointer to a time-value object."
  (if (= -1 (foreign-slot-value value 'time-value-struct
                                'microseconds))
    +now-time-value+
    (make-instance 'time-value
                   :seconds (foreign-slot-value value 'time-value-struct 'seconds)
                   :microseconds (foreign-slot-value value 'time-value-struct 'microseconds))))

(defmethod translate-to-foreign (value (type time-value-type))
  "Translate a time-value object to a foreign time-value pointer."
  (unless (ptr value)
    (let ((new-ptr (foreign-alloc 'time-value-struct)))
      (setf (ptr value) new-ptr)
      (tg:finalize value (lambda () (foreign-free new-ptr)))))
  (setf (foreign-slot-value (ptr value)
                            'time-value-struct 'seconds)
        (seconds value)
        (foreign-slot-value (ptr value)
                            'time-value-struct 'microseconds)
        (microseconds value))
  (ptr value))

(defmethod time-value-seconds ((time time-value))
  "Returns the seconds value from a time-value 'time'."
  (let ((ret (seconds time)))
    (if (= -1 ret)
      (maptime-seconds *mapped-time*)
      ret)))

(defmethod time-value-microseconds ((time time-value))
  "Returns the microseconds value from a time-value 'time'."
  (let ((ret (microseconds time)))
    (if (= -1 ret)
      (maptime-microseconds *mapped-time*)
      ret)))

(defmethod time-value-eq ((time1 time-value) (time2 time-value))
  "Return T if times are equal."
  (and (= (time-value-seconds time1)
          (time-value-seconds time2))
       (= (time-value-microseconds time1)
          (time-value-microseconds time2))))

(defmethod time-value-newer-p ((time1 time-value) (time2 time-value))
  "Returns T if time1 represents a newer time-value than time2."
  (cond
    ((time-value-eq time1 +now-time-value+) t)
    ((> (time-value-seconds time1) (time-value-seconds time2)) t)
    ((< (time-value-seconds time1) (time-value-seconds time2)) nil)
    (t
      (> (time-value-microseconds time1)
         (time-value-microseconds time2)))))

(defmethod print-object ((time time-value) stream)
  (if (time-value-eq time +now-time-value+)
    (format stream "#<time-value NOW>")
    (format stream "#<time-value seconds=~s microseconds=~s>"
            (time-value-seconds time)
            (time-value-microseconds time))))

