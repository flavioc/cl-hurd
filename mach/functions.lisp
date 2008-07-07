
(in-package :mach)

(defun port-valid (p)
  "Checks if port code is a valid port."
  (and (numberp p)
       (> p 0)))


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
  "Modify the specified port right's count of user references."
  (%mach-port-mod-refs task port right delta))


(defcfun ("mach_port_move_member" %mach-port-move-member)
  err
  (task ipc-space)
  (before port)
  (after port))

(defun port-move-member (before after
                                &optional (task (task-self)))
  "Move the specified receive right into or out of the specified port set."
  (%mach-port-move-member task before after))


(defcfun ("mach_port_insert_right" %mach-port-insert-right)
  err
  (task ipc-space)
  (port-name port)
  (right port)
  (right-type msg-type-name))

(defun port-insert-right (port right right-type
			       &optional (task (task-self)))
  "Insert the specified port right into the target task."
  (%mach-port-insert-right task
                           port
                           right
                           right-type))


(defcfun ("mach_msg_server_timeout" %mach-msg-server-timeout)
  err
  (demuxer :pointer)
  (max-size msg-size)
  (port-set port)
  (options msg-option)
  (timeout msg-timeout))

(defun msg-server-timeout (demuxer port-set &optional (timeout 0))
  "Receive RPC request messages on port-set and pass them to function demuxer."
  (%mach-msg-server-timeout demuxer 0 port-set
                            (if (> timeout 0)
                              '(:rcv-timeout)
                              '())
                            timeout))


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
  "Registers a request for a notification and supplies the send-once right notify to which the notification will be sent."
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

