
(in-package :mach)

;;
;; This file contains functions to deal with port creation.
;;

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


(defcfun ("mach_reply_port" %mach-reply-port) port)

(defun reply-port ()
  "Creates a reply port in the calling task."
  (%mach-reply-port))


(defcfun ("mach_port_allocate_name" %mach-port-allocate-name)
  err
  (task ipc-space)
  (right-type port-right)
  (name port))

(defun port-allocate-name (right-type port-name &optional (task (task-self)))
  "Creates a new right in the specified task, with a specified name for the new right. name must not already be in use for some right, and it can't be the reserved values nil or :dead. On success the port-name is returned."
  (let ((return-code
          (%mach-port-allocate-name task right-type port-name)))
    (select-error return-code
                  port-name)))
