
(in-package :hurd)

(defcfun ("file_check_access" %file-check-access)
  err
  (file port)
  (allowed :pointer))

(defconstant +allowed-file-check-access-flags+ '(:read :write :exec))

(defun file-check-access (file)
  (declare (type fixnum file))
  (with-foreign-pointer (allowed (foreign-type-size 'open-flags))
    (let ((err (%file-check-access file allowed)))
      (select-error err
                    (only-flags (mem-ref allowed 'open-flags)
                                +allowed-file-check-access-flags+)))))
