
(in-package :mach)

(defcfun ("mach_port_get_receive_status" %mach-port-get-receive-status)
  err
  (task ipc-space)
  (name port)
  (status :pointer))

(defun port-get-receive-status (name &optional (task (task-self)))
  "Returns the current status of the specified receive right."
  (with-foreign-pointer (status (foreign-type-size :pointer))
    (let ((return-code
            (%mach-port-get-receive-status task
                                           name
                                           status)))
      (select-error return-code
                    (make-port-status status)))))

(defcfun ("mach_port_set_mscount" %mach-port-set-mscount)
  err
  (task ipc-space)
  (name port)
  (mscount port-mscount))

(defun port-set-mscount (port count &optional (task (task-self)))
  (declare (type fixnum count))
  (%mach-port-set-mscount task
                          port
                          count))

(defcfun ("mach_port_set_qlimit" %mach-port-set-qlimit)
  err
  (task ipc-space)
  (name port)
  (qlimit port-msgcount))

(defconstant +qlimit-default+ 5 "Queue default size.")
(defconstant +qlimit-min+ 0 "Queue minimum value.")
(defconstant +qlimit-max+ 16 "Queue maximum size.")

(defun port-set-qlimit (port &optional (limit +qlimit-default+) (task (task-self)))
  "Changes the queue limit task's receive right named name to qlimit."
  (declare (type fixnum limit))
  (assert (and (>= limit +qlimit-min+) (<= limit +qlimit-max+)))
  (%mach-port-set-qlimit task port limit))

(defcfun ("mach_port_set_seqno" %mach-port-set-seqno)
  err
  (task ipc-space)
  (name port)
  (seqno port-seqno))

(defun port-set-seqno (port seqno &optional (task (task-self)))
  "Changes the sequence number task's receive right named port to seqno."
  (declare (type fixnum seqno))
  (%mach-port-set-seqno task port seqno))
