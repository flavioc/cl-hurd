
(defcfun ("getauth" %getauth) auth)

(defun getauth ()
  (%getauth))
