
(in-package :mach)

(defcfun ("mach_task_self" %mach-task-self) ipc-space)

(defun task-self ()
  "Returns a send right associated with the task_self port"
  (%mach-task-self))

(defcfun ("task_get_special_port" %task-get-special-port)
		 err
		 (task task)
		 (what special-port-type)
		 (port port-pointer))

(defun task-get-special-port (what &optional (task (task-self)))
  "Return a send write to the indicated special port."
  (with-foreign-object (port 'port)
    (let ((return-code (%task-get-special-port task what port)))
      (select-error return-code (mem-ref port 'port)))))

(defun task-get-bootstrap-port (&optional (task (task-self)))
  "Return a send write to the bootstrap port."
  (task-get-special-port :task-bootstrap-port task))

