
(in-package :hurd)

(define-stub-library fsys-reply)

(defcfun ("fsys_goaway_reply" %fsys-goaway-reply)
  err
  (reply port)
  (reply-type msg-type-name)
  (ret-code err))

(defun fsys-goaway-reply (reply reply-type ret-code)
  (%fsys-goaway-reply reply reply-type ret-code))
