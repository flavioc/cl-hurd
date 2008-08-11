
(in-package :hurd)

(defcfun ("io_duplicate" %io-duplicate)
  err
  (file port)
  (newport port-pointer))

(defun io-duplicate (file)
  "Duplicate the file port."
  (declare (type fixnum file))
  (with-foreign-pointer (newport (foreign-type-size 'port))
    (select-error (%io-duplicate file newport)
                  (mem-ref newport 'port))))

