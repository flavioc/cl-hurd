
(in-package :hurd)

(define-helper-library file-utimes)

(defcfun ("helper_file_utimes" %file-utimes)
  err
  (file port)
  (new-atime time-value-t)
  (new-mtime time-value-t))

(defun file-utimes (file &key (atime :now) (mtime :now))
  (declare (type fixnum file))
  (when (eq atime :now)
    (setf atime +now-time-value+))
  (when (eq mtime :now)
    (setf mtime +now-time-value+))
  (let ((err (%file-utimes file atime mtime)))
    (select-error err t)))

