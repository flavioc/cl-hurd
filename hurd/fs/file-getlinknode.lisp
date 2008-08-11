
(in-package :hurd)

(defcfun ("file_getlinknode" %file-getlinknode)
  err
  (file port)
  (linknode port-pointer))

(defun file-getlinknode (file)
  "Get a node for hard links from 'file'."
  (declare (type fixnum file))
  (with-foreign-pointer (linknode (foreign-type-size 'port))
    (select-error (%file-getlinknode file linknode)
                  (mem-ref linknode 'port))))
