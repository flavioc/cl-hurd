
(in-package :hurd)

(defcfun ("fsys_goaway" fsys-goaway)
  err
  (control port)
  (flags fsys-goaway-flags))
