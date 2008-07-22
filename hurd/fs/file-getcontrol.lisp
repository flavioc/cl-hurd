
(in-package :hurd)

(defcfun ("file_getcontrol" %file-getcontrol)
  err
  (file port)
  (control port-pointer))

(defun file-getcontrol (file)
  (declare (type fixnum file))
  (with-foreign-pointer (control (foreign-type-size 'port))
    (let ((err (%file-getcontrol file control)))
      (select-error err
                    (mem-ref control 'port)))))
