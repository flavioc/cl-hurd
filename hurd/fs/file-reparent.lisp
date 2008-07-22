
(in-package :hurd)

(defcfun ("file_reparent" %file-reparent)
  err
  (file port)
  (parent port)
  (new-file port-pointer))

(defun file-reparent (file parent)
  (declare (type fixnum file parent))
  (with-foreign-pointer (new-file (foreign-type-size 'port))
    (let ((err (%file-reparent file parent new-file)))
      (select-error err
                    (mem-ref new-file 'port)))))
