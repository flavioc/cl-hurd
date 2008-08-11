
(in-package :hurd)

(defcfun ("getcwdir" %getcwdir) file-t)

(defun getcwdir ()
  "Get a file port name of current working directory."
  (%getcwdir))

