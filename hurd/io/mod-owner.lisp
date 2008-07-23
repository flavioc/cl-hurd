
(in-package :hurd)

(defcfun ("io_mod_owner" %io-mod-owner)
  err
  (file port)
  (owner pid-t))

(defun io-mod-owner (file owner)
  (declare (type fixnum file owner))
  (select-error (%io-mod-owner file owner)))

