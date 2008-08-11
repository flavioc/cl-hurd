
(in-package :hurd)

(defcfun ("io_revoke" %io-revoke)
  err
  (file port))

(defun io-revoke (file)
  "Revoke all ports refered to 'file', except this one."
  (declare (type fixnum file))
  (select-error (%io-revoke file)))

