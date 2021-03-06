
(in-package :hurd)

(defcfun ("file_check_access" %file-check-access)
  err
  (file port)
  (allowed :pointer))

(defconstant +allowed-file-check-access-flags+ '(:read :write :exec))

(defun file-check-access (file)
  "Returns the allowed access flags related to 'file'."
  (declare (type fixnum file))
  (with-foreign-pointer (allowed (foreign-type-size 'open-flags))
    (select-error (%file-check-access file allowed)
				  (only-flags (mem-ref allowed 'open-flags)
							  +allowed-file-check-access-flags+))))
