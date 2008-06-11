
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
  (%mach-port-deallocate task name))

(defmacro with-port ((port-name creation &key (task 'self)) &body body)
  `(let ((,port-name ,creation))
	 (when (port-valid ,port-name)
	   ,@body
	   (port-deallocate ,port-name ,@(unless (eq task 'self) (list task))))))
