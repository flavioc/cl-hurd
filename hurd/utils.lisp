
(in-package :hurd)

(defun %new-ptr ()
  (foreign-alloc :pointer))

(defun %new-unsigned (n)
  (foreign-alloc :unsigned-int :initial-element n))

