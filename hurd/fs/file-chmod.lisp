
(in-package :hurd)

(defcfun ("file_chmod" %file-chmod)
  err
  (file port)
  (mode mode-t))

(defun file-chmod (file new-mode)
  "Change permission bits of 'file'."
  (declare (type fixnum file)
           (type mode new-mode))
  (select-error (%file-chmod file new-mode)))
