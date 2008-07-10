
(in-package :mach)

(defcfun ("mach_port_request_notification" %mach-port-request-notification)
  err
  (task ipc-space)
  (name port)
  (variant msg-notify)
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

