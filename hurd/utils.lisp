
(in-package :hurd)

(defun %new-ptr ()
  "Allocate a new pointer to pointer."
  (foreign-alloc :pointer))

(defun %new-unsigned (n)
  "Allocate a new unsigned point to 'n' value."
  (foreign-alloc :unsigned-int :initial-element n))

