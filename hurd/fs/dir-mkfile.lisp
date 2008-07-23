
(in-package :hurd)

(defcfun ("dir_mkfile" %dir-mkfile)
  err
  (dir port)
  (flags open-flags)
  (mode mode-t)
  (newnode port-pointer))

(defun dir-mkfile (dir &optional (flags nil) (mode (make-mode)))
  (declare (type fixnum dir)
           (type list flags)
           (type mode mode))
  (with-foreign-pointer (newnode (foreign-type-size 'port))
    (let ((err (%dir-mkfile dir flags mode newnode)))
      (select-error err
                    (mem-ref newnode 'port)))))

