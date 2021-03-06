
(in-package :hurd)

(defcfun ("dir_link" %dir-link)
  err
  (dir port)
  (file port)
  (name :string)
  (excl :boolean))

(defun dir-link (dir &key file name (excl nil))
  "Create an hard link in dir for file 'file' with name 'name'."
  (declare (type fixnum dir file)
           (type string name)
           (type boolean excl))
  (select-error (%dir-link dir file name excl)))
