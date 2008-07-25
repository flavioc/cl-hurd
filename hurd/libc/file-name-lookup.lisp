
(in-package :hurd)

(defcfun ("file_name_lookup" %file-name-lookup)
  file-t
  (name :string)
  (flags open-flags)
  (mode mode-t))

(defcfun ("file_name_lookup_under" %file-name-lookup-under)
  file-t
  (startdir file-t)
  (name :string)
  (flags open-flags)
  (mode mode-t))

(defun file-name-lookup (name &key
                              (flags nil)
                              (mode nil)
                              (under nil))
  "Open a port to file 'name'."
  (cond
    (under (%file-name-lookup-under under name flags mode))
    (t (%file-name-lookup name flags mode))))
