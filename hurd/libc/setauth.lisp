
(in-package :hurd)

(defcfun ("setauth" %setauth)
  :int
  (auth auth-t))

(defun setauth (auth)
  "Set current authentication server."
  (declare (type fixnum auth))
  (let ((err (%setauth auth)))
    (not (= err -1))))

