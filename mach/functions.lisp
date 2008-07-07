
(in-package :mach)

(defun port-valid (p)
  "Checks if port code is a valid port."
  (and (numberp p)
       (> p 0)))

(defcfun ("mach_port_move_member" %mach-port-move-member)
  err
  (task ipc-space)
  (before port)
  (after port))

(defun port-move-member (before after
                                &optional (task (task-self)))
  "Move the specified receive right into or out of the specified port set."
  (%mach-port-move-member task before after))


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

