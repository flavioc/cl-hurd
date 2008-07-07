
(in-package :mach)

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


(defcfun ("mach_port_extract_right" %mach-port-extract-right)
  err
  (task ipc-space)
  (name port)
  (desired-type msg-type-name)
  (right :pointer)
  (acquired-type :pointer))

(defun port-extract-right (name desired-type &optional (task (task-self)))
  "Extracts a port right from the target task and returns it to the caller as if the task sent the right voluntarily."
  (with-foreign-pointer (right (foreign-type-size 'port))
    (with-foreign-pointer (acquired-type (foreign-type-size 'msg-type-name))
      (let ((return-code
              (%mach-port-extract-right task
                                        name
                                        desired-type
                                        right
                                        acquired-type)))
        (select-error return-code
                      (values (mem-ref right 'port)
                              (mem-ref acquired-type 'msg-type-name)))))))

