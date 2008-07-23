
(in-package :hurd)

(defcfun ("io_pathconf" %io-pathconf)
  err
  (file port)
  (name pathconf-type)
  (value :pointer))

(defun io-pathconf (file name)
  (declare (type fixnum file)
           (type keyword name))
  (with-foreign-pointer (value (foreign-type-size :int))
    (select-error (%io-pathconf file name value)
                  (mem-ref value :int))))

