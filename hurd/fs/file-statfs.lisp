
(in-package :hurd)

(defcfun ("file_statfs" %file-statfs)
  err
  (file port)
  (info :pointer))

(defun file-statfs (file)
  (declare (type fixnum file))
  (with-foreign-pointer (info (foreign-type-size 'statfs-struct))
    (let ((err (%file-statfs file info)))
      (select-error err
                    (let ((obj (make-statfs))
                          (copy (make-statfs info)))
                      (statfs-copy obj copy)
                      obj)))))
