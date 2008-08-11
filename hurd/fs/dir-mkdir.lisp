
(in-package :hurd)

(defcfun ("dir_mkdir" %dir-mkdir)
  err
  (dir port)
  (name :string)
  (mode mode-t))

(defun dir-mkdir (dir name &optional
					  (mode (make-mode :perms '((:owner :exec :read :write)
												(:group :exec :read :write)))))
  "Create a new directory with name 'name' in directory 'dir'."
  (declare (type fixnum dir)
           (type string name))
  (select-error (%dir-mkdir dir name mode)))

