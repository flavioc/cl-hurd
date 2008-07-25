
(in-package :hurd)

(defcfun ("getauth" %getauth) auth-t)

(defun getauth ()
  "Get port name of current authentication server."
  (%getauth))
