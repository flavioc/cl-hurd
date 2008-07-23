
(in-package :hurd)

(defcfun ("io_revoke" %io-revoke)
  err
  (file port))

(defun io-revoke (file)
  (declare (type fixnum file))
  (select-error (%io-revoke file)))

