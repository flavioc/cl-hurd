
(in-package :hurd)

(defcfun ("file_sync" %file-sync)
  err
  (file port)
  (wait :boolean)
  (omit-metadata :boolean))

(defun file-sync (file &key (wait t) (omit-metadata nil))
  "Sync 'file'. Pass T to 'wait' if you want to wait. 'omit-metadata' tells if you also want to sync the file metadata (stat information, etc)."
  (declare (type fixnum file)
           (type boolean wait omit-metadata))
  (select-error (%file-sync file wait omit-metadata)))
