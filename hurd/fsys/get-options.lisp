
(in-package :hurd)

(defcfun ("fsys_get_options" %fsys-get-options)
  err
  (fsys port)
  (data :pointer)
  (data-len :pointer))

(defun fsys-get-options (fsys)
  (declare (type fixnum fsys))
  (with-foreign-pointer (options (foreign-type-size :pointer))
    (with-foreign-pointer (options-len (foreign-type-size 'msg-type-number))
      (setf (mem-ref options-len 'msg-type-number) 0)
      (select-error (%fsys-get-options fsys
                                       options
                                       options-len)
                    (get-foreign-options (mem-ref options :pointer)
                                         (mem-ref options-len 'msg-type-number))))))
