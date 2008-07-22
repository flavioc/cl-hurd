
(in-package :hurd)

(defcfun ("file_sync" %file-sync)
  err
  (file port)
  (wait :boolean)
  (omit-metadata :boolean))

(defun file-sync (file &key (wait t) (omit-metadata nil))
  (declare (type fixnum file)
           (type boolean wait omit-metadata))
  (select-error (%file-sync file wait omit-metadata)))
