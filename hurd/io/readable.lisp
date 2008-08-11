
(in-package :hurd)

(defcfun ("io_readable" %io-readable)
  err
  (file port)
  (amount :pointer))

(defun io-readable (file)
  "Tells how much bytes are there left to be read from 'file'."
  (declare (type fixnum file))
  (with-foreign-pointer (amount (foreign-type-size 'vm-size))
    (select-error (%io-readable file amount)
                  (mem-ref amount 'vm-size))))

