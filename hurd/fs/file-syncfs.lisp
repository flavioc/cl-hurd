
(in-package :hurd)

(defcfun ("file_syncfs" %file-syncfs)
  err
  (file port)
  (wait :boolean)
  (do-children :boolean))

(defun file-syncfs (file &key (wait t) (do-children t))
  "Sync the entire filesystem. 'wait' tells if you want to wait, and 'do-children' if you also want to sync children translators."
  (declare (type fixnum file))
  (select-error (%file-syncfs file wait do-children)))
