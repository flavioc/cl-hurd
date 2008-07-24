
(in-package :hurd)

(defcfun ("fsys_goaway" %fsys-goaway)
  err
  (control port)
  (flags fsys-goaway-flags))

(defun fsys-goaway (control flags)
  (declare (type fixnum control)
           (type list flags))
  (let ((err (%fsys-goaway control flags)))
    (when (or (eq err :server-died)
              (eq err :send-invalid-dest))
      (setf err t))
    (select-error err)))
