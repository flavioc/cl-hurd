
(in-package :hurd)

(defcfun ("helper_file_utimes" %file-utimes)
  err
  (file port)
  (new-atime time-value-t)
  (new-mtime time-value-t))

(defun file-utimes (file &key (atime +now-time-value+) (mtime +now-time-value+))
  "Change access time and/or modification time to 'file'."
  (declare (type fixnum file))
  (when (eq atime :now)
    (setf atime +now-time-value+))
  (when (eq mtime :now)
    (setf mtime +now-time-value+))
  (select-error (%file-utimes file atime mtime)))

