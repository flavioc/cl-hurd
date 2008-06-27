
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

;; port-mod-refs

(defcfun ("mach_port_mod_refs" %mach-port-mod-refs)
	 err
	 (task ipc-space)
	 (port-name port)
	 (right port-right)
	 (delta port-delta))

(defun port-mod-refs (port right
			   &optional
			   (delta -1) ; drop 1 ref by default
			   (task (task-self)))
  (%mach-port-mod-refs task port right delta))

;; port-move-member

(defcfun ("mach_port_move_member" %mach-port-move-member)
	 err
	 (task ipc-space)
	 (before port)
	 (after port))

(defun port-move-member (before after
				&optional (task (task-self)))
  (%mach-port-move-member task before after))

;; port-insert-right

(defcfun ("mach_port_insert_right" %mach-port-insert-right)
	 err
	 (task ipc-space)
	 (port-name port)
	 (right port)
	 (right-type msg-type-name))

(defun port-insert-right (port right right-type
			       &optional (task (task-self)))
  (%mach-port-insert-right task
			   port
			   right
			   right-type))

;; port-type

(defcfun ("mach_port_type" %mach-port-type)
	 err
	 (task ipc-space)
	 (name port)
	 (ptype :pointer))

(defun port-type (port &optional (task (task-self)))
  (with-foreign-object (ptype 'port-type-t)
    (let ((return-code
	    (%mach-port-type task port ptype)))
      (select-error return-code (mem-ref ptype 'port-type-t)))))

;; msg-server-timeout

(defcfun ("mach_msg_server_timeout" %mach-msg-server-timeout)
	 err
	 (demuxer :pointer)
	 (max-size :int)
	 (port-set port)
	 (options :int)
	 (timeout :unsigned-int))

(defconstant +timeout-none+ 0)
(defconstant +timeout-option+ #x00000100)

(defun msg-server-timeout (demuxer port-set &optional (timeout 0))
  (%mach-msg-server-timeout demuxer 0 port-set
			    (if (> timeout 0)
			      +timeout-option+
			      +timeout-none+)
			    timeout))

;; port-request-notification

(defcfun ("mach_port_request_notification" %mach-port-request-notification)
	 err
	 (task ipc-space)
	 (name port)
	 (variant msg-id)
	 (sync port-mscount)
	 (notify port)
	 (notify-type msg-type-name)
	 (previous :pointer))

(defun port-request-notification (port variant sync notify notify-type
				       &optional (task (task-self)))
  (with-foreign-object (foo 'port)
     (let ((error-code (%mach-port-request-notification task
				      port
				      variant
				      sync
				      notify
				      notify-type
				      foo)))
      (select-error error-code
		    (let ((foo-ref (mem-ref foo 'port)))
		      (when (port-valid foo-ref)
			(port-deallocate foo-ref task)
			t))))))
