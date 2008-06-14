
; mach_task_self
(defcfun ("mach_task_self" %mach-task-self) ipc-space)

(defun task-self ()
  "Returns a send right associated with the task_self port"
  (%mach-task-self))

(defun port-valid (p)
  (and (numberp p)
       (> p 0)))

(defcfun ("mach_port_deallocate" %mach-port-deallocate)
		 err
		 (task ipc-space)
		 (name port))

(defun port-deallocate (name &optional (task (task-self)))
  "Deallocates a port in a task ipc namespace"
  (%mach-port-deallocate task name))

(defmacro with-port ((port-name creation &key (task 'self)) &body body)
  `(let ((,port-name ,creation))
	 (when (port-valid ,port-name)
	   ,@body
	   (port-deallocate ,port-name ,@(unless (eq task 'self) (list task))))))

(defcfun ("task_get_special_port" %task-get-special-port)
		 err
		 (task task)
		 (what :int)
		 (port :pointer))

; task_get_bootstrap_port appears to be a macro
; that uses task_get_special_port with TASK_BOOTSTRAP_PORT
; as the 'what' argument
; this is defined at mach/task_special_ports.h
(defconstant +task-bootstrap-port+ 4)

(defun get-bootstrap-port (&key (task (task-self)))
  (with-foreign-object (bootstrap 'port)
    (%task-get-special-port task +task-bootstrap-port+ bootstrap)
	(translate-from-foreign (mem-ref bootstrap 'port) 'port)))
