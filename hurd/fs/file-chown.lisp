
(in-package :hurd)

(defcfun ("file_chown" %file-chown)
  err
  (file port)
  (new-owner uid-t)
  (new-group gid-t))

(defun file-chown (file new-owner new-group)
  (declare (type fixnum file new-owner new-group))
  (select-error (%file-chown file new-owner new-group)))
