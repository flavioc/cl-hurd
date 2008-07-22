
(in-package :hurd)

(defcfun ("file_chmod" %file-chmod)
  err
  (file port)
  (mode mode-t))

(defun file-chmod (file new-mode)
  (declare (type fixnum file)
           (type mode new-mode))
  (let ((err (%file-chmod file new-mode)))
    (select-error err t)))
