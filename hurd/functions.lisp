
(in-package :hurd)

(defcfun ("getauth" %getauth) auth-t)

(defun getauth ()
  "Get port name of current authentication server."
  (%getauth))


(defcfun ("file_name_lookup" %file-name-lookup)
  port
  (name :string)
  (flags open-flags)
  (mode mode-t))

(defun file-name-lookup (name &optional (flags nil) (mode nil))
  "Open a port to file 'name'."
  (%file-name-lookup name flags mode))
