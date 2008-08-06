
(in-package :hurd)

(defcfun ("dir_mkfile" %dir-mkfile)
  err
  (dir port)
  (flags open-flags)
  (mode mode-t)
  (newnode port-pointer))

(defun dir-mkfile (dir &key (flags nil) (mode (make-mode)))
  (declare (type fixnum dir)
           (type list flags)
           (type mode mode))
  (with-foreign-pointer (newnode (foreign-type-size 'port))
    (select-error (%dir-mkfile dir flags mode newnode)
                  (mem-ref newnode 'port))))

