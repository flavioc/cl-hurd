
(in-package :mach)

;;
;; This file implements all the port destruction functions.
;;

(defcfun ("mach_port_deallocate" %mach-port-deallocate)
		 err
		 (task ipc-space)
		 (name port))

(defun port-deallocate (name &optional (task (task-self)))
  "Deallocates a port in a task ipc namespace"
  (%mach-port-deallocate task name))


(defcfun ("mach_port_destroy" %mach-port-destroy)
  err
  (task ipc-space)
  (port-name port))

(defun port-destroy (port-name &optional (task (task-self)))
  "Deallocates all rights denoted by a name. The name becomes immediately available for reuse."
  (%mach-port-destroy task port-name))


