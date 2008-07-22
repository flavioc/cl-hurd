
(in-package :hurd)

(defcfun ("dir_rename" %dir-rename)
  err
  (dir port)
  (oldname :string)
  (new-dir port)
  (newname :string)
  (excl :boolean))

(defun dir-rename (old-dir new-dir &key oldname newname (excl nil))
  (declare (type fixnum old-dir new-dir)
           (type string oldname newname)
           (type boolean excl))
  (select-error (%dir-rename old-dir oldname new-dir newname excl)))

