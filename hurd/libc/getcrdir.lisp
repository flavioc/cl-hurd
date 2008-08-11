
(in-package :hurd)

(defcfun ("getcrdir" %getcrdir) file-t)

(defun getcrdir ()
  "Get a file port name of current root directory."
  (%getcrdir))

