
(in-package :hurd)

(defcfun ("dir_rmdir" %dir-rmdir)
  err
  (dir port)
  (name :string))

(defun dir-rmdir (dir name)
  (declare (type fixnum dir)
           (type string name))
  (select-error (%dir-rmdir dir name)))
