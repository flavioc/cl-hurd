
(in-package :hurd)

(defcfun ("setcwdir" %setcwdir)
  :int
  (file file-t))

(defun setcwdir (file)
  "Change current directory to 'file' port."
  (declare (type fixnum file))
  (let ((err (%setcwdir file)))
    (and (numberp err)
         (zerop err))))

