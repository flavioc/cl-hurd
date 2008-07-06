
(in-package :mach)

(defcfun ("vm_allocate" %vm-allocate)
  err
  (target-task task)
  (address vm-address)
  (size vm-size)
  (anywhere :boolean))

(defun vm-allocate (address size anywhere &optional (task (task-self)))
  "Allocate a region of virtual memory."
  (%vm-allocate task address size anywhere))
