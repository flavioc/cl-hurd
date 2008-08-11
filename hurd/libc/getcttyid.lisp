
(in-package :hurd)

(defcfun ("getcttyid" %getcttyid) port)

(defun getcttyid ()
  "Get the CTTY port."
  (%getcttyid))

