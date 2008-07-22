
(in-package :hurd)

(defcfun ("file_syncfs" %file-syncfs)
  err
  (file port)
  (wait :boolean)
  (do-children :boolean))

(defun file-syncfs (file &key (wait t) (do-children t))
  (declare (type fixnum file))
  (select-error (%file-syncfs file wait do-children)))
