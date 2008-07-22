
(in-package :hurd)

(defcfun ("file_set_size" %file-set-size)
  err
  (file port)
  (new-size loff-t))

(defun file-set-size (file new-size)
  (declare (type fixnum file)
           (type integer new-size)) ; new-size may be bigger than fixnum (long long)
  (let ((err (%file-set-size file new-size)))
    (select-error err t)))

