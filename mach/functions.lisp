
; mach_task_self
(defcfun ("mach_task_self" %mach-task-self) port)

(defun task-self ()
  "Returns a send right associated with the task_self port"
  (%mach-task-self))

(defun port-valid (p)
  (numberp p))
