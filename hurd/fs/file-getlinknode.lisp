
(in-package :hurd)

(defcfun ("file_getlinknode" %file-getlinknode)
  err
  (file port)
  (linknode port-pointer))

(defun file-getlinknode (file)
  (declare (type fixnum file))
  (with-foreign-pointer (linknode (foreign-type-size 'port))
    (let ((err (%file-getlinknode file linknode)))
      (select-error err
                    (mem-ref linknode 'port)))))
