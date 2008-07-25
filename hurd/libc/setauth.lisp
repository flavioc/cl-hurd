
(in-package :hurd)

(defcfun ("setauth" %setauth)
  :int
  (auth auth-t))

(defun setauth (auth)
  (declare (type fixnum auth))
  (let ((err (%setauth auth)))
    (not (= err -1))))
