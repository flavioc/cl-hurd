
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

(defcfun ("vm_deallocate" %vm-deallocate)
  err
  (task vm-task)
  (address vm-address)
  (size vm-size))

(defun vm-deallocate (address size &optional (task (task-self)))
  "Relinquishes access to a region of a task's address space, causing further access to that memory to fail."
  (%vm-deallocate task address size))
