
(in-package :hurd)

(defcfun ("file_lock" %file-lock)
  err
  (file port)
  (flags lock-flags))

(defun file-lock (file flags)
  "Lock file 'file' with flags 'flags'."
  (declare (type fixnum file)
           (type list flags))
  (select-error (%file-lock file flags)))
