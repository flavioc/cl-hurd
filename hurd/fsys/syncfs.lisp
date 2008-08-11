
(in-package :hurd)

(defcfun ("fsys_syncfs" %fsys-syncfs)
  err
  (fsys port)
  (wait :boolean)
  (do-children :boolean))

(defun fsys-syncfs (fsys &key (wait t) (do-children t))
  "Sync an entire filesystem. Same as file-syncfs."
  (declare (type fixnum fsys)
           (type boolean wait do-children))
  (select-error (%fsys-syncfs fsys wait do-children)))

