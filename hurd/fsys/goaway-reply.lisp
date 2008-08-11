
(in-package :hurd)

(defcfun ("fsys_goaway_reply" %fsys-goaway-reply)
  err
  (reply port)
  (reply-type msg-type-name)
  (ret-code err))

(defun fsys-goaway-reply (reply reply-type ret-code)
  "Sends a fsys-goaway reply."
  (%fsys-goaway-reply reply reply-type ret-code))
