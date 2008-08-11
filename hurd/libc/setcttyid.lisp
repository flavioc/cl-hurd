
(in-package :hurd)

(defcfun ("setcttyid" %setcttyid)
  :int
  (port port))

(defun setcttyid (port)
  "Set the CTTY port."
  (declare (type fixnum port))
  (let ((err (%setcttyid port)))
    (not (= err -1))))

