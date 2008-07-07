
(in-package :mach)

(defcfun ("mach_port_get_set_status" %mach-port-get-set-status)
  err
  (task ipc-space)
  (name port)
  (members :pointer)
  (count :pointer))

(defun port-get-set-status (port &optional (task (task-self)))
  "Returns the members of a port set."
  (with-foreign-pointer (members (foreign-type-size :pointer))
    (with-foreign-pointer (count (foreign-type-size 'msg-type-number))
      (let ((return-code
              (%mach-port-get-set-status task
                                         port
                                         members
                                         count)))
        (select-error return-code
                      (with-cleanup (munmap (mem-ref members :pointer)
                                            (* (mem-ref count 'msg-type-number)
                                               (foreign-type-size 'port)))
                        (%foreign-port-array-to-list
                          (mem-ref members :pointer)
                          (mem-ref count 'msg-type-number))))))))

(defun %foreign-port-array-to-list (ptr cnt)
  "Converts a port array to a list."
  (loop for i from 0 below cnt
        collect (mem-aref ptr 'port i)))

(defcfun ("mach_port_move_member" %mach-port-move-member)
  err
  (task ipc-space)
  (before port)
  (after port))

(defun port-move-member (before after &optional (task (task-self)))
  "Move the specified receive right into or out of the specified port set."
  (%mach-port-move-member task before after))

