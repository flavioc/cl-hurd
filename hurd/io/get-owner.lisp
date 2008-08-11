
(in-package :hurd)

(defcfun ("io_get_owner" %io-get-owner)
  err
  (file port)
  (owner :pointer))

(defun io-get-owner (file)
  "Get the file owner."
  (declare (type fixnum file))
  (with-foreign-pointer (owner (foreign-type-size 'pid-t))
    (select-error (%io-get-owner file owner)
                  (mem-ref owner 'pid-t))))
