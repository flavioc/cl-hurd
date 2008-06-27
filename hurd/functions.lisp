
(defcfun ("getauth" %getauth) auth)

(defun getauth ()
  (%getauth))

(defcfun ("file_name_lookup" %file-name-lookup)
	 port
	 (name :string)
	 (flags open-flags-t)
	 (mode mode-t))

(defun file-name-lookup (name &optional (flags nil) (mode nil))
  (%file-name-lookup name flags mode))
