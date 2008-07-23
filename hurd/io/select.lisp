
(in-package :hurd)

(defcfun ("io_select" %io-select)
  err
  (file port)
  (reply port)
  (timeout :unsigned-int)
  (select-type :pointer))

(defun io-select (file &key type
                       reply (timeout 0))
  (declare (type fixnum file timeout)
           (type list type))
  (with-foreign-pointer (ptr (foreign-type-size 'select-type))
    (setf (mem-ref ptr 'select-type) type)
    (select-error (%io-select file reply timeout ptr)
                  (mem-ref ptr 'select-type))))

