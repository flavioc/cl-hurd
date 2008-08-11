
(in-package :hurd)

(defcfun ("dir_unlink" %dir-unlink)
  err
  (dir port)
  (name :string))

(defun dir-unlink (dir name)
  "Unlink file 'name' from directory 'dir'."
  (declare (type fixnum dir)
           (type string name))
  (select-error (%dir-unlink dir name)))
