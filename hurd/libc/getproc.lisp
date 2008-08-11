
(in-package :hurd)

(defcfun ("getproc" %getproc) process-t)

(defun getproc ()
  "Get port name of current process server."
  (%getproc))

