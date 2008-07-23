
(in-package :hurd)

(defcfun ("io_seek" %io-seek)
  err
  (file port)
  (offset loff-t)
  (whence seek-type)
  (newp :pointer))

(defun io-seek (file &key (offset 1) (whence :seek-cur))
  (declare (type fixnum file)
           (type integer offset)
           (type symbol whence))
  (with-foreign-pointer (newp (foreign-type-size 'loff-t))
    (select-error (%io-seek file offset whence newp)
                  (mem-ref newp 'loff-t))))
