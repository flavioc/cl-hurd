
(in-package :hurd)

(defcfun ("file_get_fs_options" %file-get-fs-options)
  err
  (file port)
  (options :pointer)
  (options-len :pointer))

(defun file-get-fs-options (file)
  (declare (type fixnum file))
  (with-foreign-pointer (options (foreign-type-size :pointer))
    (with-foreign-pointer (options-len (foreign-type-size 'msg-type-number))
      (setf (mem-ref options-len 'msg-type-number) 0)
      (let ((err (%file-get-fs-options file
                                       options
                                       options-len)))
        (select-error err
                      (foreign-string-zero-separated-to-list
                        (mem-ref options :pointer)
                        (mem-ref options-len 'msg-type-number)))))))