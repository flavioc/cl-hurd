
;; task-self
;;
; mach_task_self
(defcfun ("mach_task_self" %mach-task-self) ipc-space)

(defun task-self ()
  "Returns a send right associated with the task_self port"
  (%mach-task-self))

;; port-valid

(defun port-valid (p)
  "Checks if port code is a valid port"
  (and (numberp p)
       (> p 0)))

;; port-deallocate
;; deals with port deallocation

(defcfun ("mach_port_deallocate" %mach-port-deallocate)
		 err
		 (task ipc-space)
		 (name port))

(defun port-deallocate (name &optional (task (task-self)))
  "Deallocates a port in a task ipc namespace"
  (%mach-port-deallocate task name))


;; with-port: ease up port managing

(defmacro with-port ((port-name creation &optional (task nil))
					 &body body)
  `(let ((,port-name ,creation))
	 (when (port-valid ,port-name)
	   (with-cleanup (port-deallocate ,port-name ,(if (null task)
							`(task-self)
							task))
	     ,@body))))

;; get-bootstrap-port

(defcfun ("task_get_special_port" %task-get-special-port)
		 err
		 (task task)
		 (what special-port-type)
		 (port port-pointer))

; task_get_bootstrap_port appears to be a macro
; that uses task_get_special_port with TASK_BOOTSTRAP_PORT

(defun get-bootstrap-port (&optional (task (task-self)))
  (with-foreign-object (bootstrap 'port)
    (let ((return-code 
	    (%task-get-special-port task :task-bootstrap-port bootstrap)))
	  (select-error return-code (mem-ref bootstrap 'port)))))

;; port-allocate

(defcfun ("mach_port_allocate" %mach-port-allocate)
	 err
	(task ipc-space)
	(right port-right)
	(ret-port port-pointer))

(defun port-allocate (right &optional (task (task-self)))
  "Creates a new right in the specified task."
  (with-foreign-object (port-name 'port)
    (let ((return-code
	    (%mach-port-allocate task right port-name)))
	  (select-error return-code (mem-ref port-name 'port)))))

;; port-destroy 

(defcfun ("mach_port_destroy" %mach-port-destroy)
	 err
	 (task ipc-space)
	 (port-name port))

(defun port-destroy (port-name &optional (task (task-self)))
  "Deallocates all rights denoted by a name. The name becomes immediately available for reuse."
  (%mach-port-destroy task port-name))
