
(in-package :hurd)

(defcfun ("dir_rename" %dir-rename)
  err
  (dir port)
  (oldname :string)
  (new-dir port)
  (newname :string)
  (excl :boolean))

(defun dir-rename (old-dir new-dir &key oldname newname (excl nil))
  "Rename file 'oldname' in directory 'old-dir' to 'newname' to directory 'new-dir'."
  (declare (type fixnum old-dir new-dir)
           (type string oldname newname)
           (type boolean excl))
  (let ((err (%dir-rename old-dir oldname new-dir newname excl)))
    (select-error err)))

