
(in-package :hurd)

(defcfun ("file_chauthor" %file-chauthor)
  err
  (file port)
  (new pid-t))

(defun file-chauthor (file new)
  (declare (type fixnum file new))
  (select-error (%file-chauthor file new)))
