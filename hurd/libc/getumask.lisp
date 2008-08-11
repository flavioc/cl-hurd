
(in-package :hurd)

(defcfun ("getumask" %getumask) mode-t)

(defun getumask ()
  "Get the current `umask' value without changing it."
  (%getumask))

