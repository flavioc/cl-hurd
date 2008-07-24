
(in-package :hurd)

(defcfun ("file_getcontrol" %file-getcontrol)
  err
  (file port)
  (control port-pointer))

(defun file-getcontrol (file)
  (declare (type fixnum file))
  (with-foreign-pointer (control (foreign-type-size 'port))
    (select-error (%file-getcontrol file control)
				  (mem-ref control 'port))))
