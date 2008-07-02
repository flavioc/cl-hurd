
(define-stub-library notify)

(defcenum notify-routine-code
	:do-mach-notify-port-deleted
	:do-mach-notify-msg-accepted
	:do-mach-notify-port-destroyed
	:do-mach-notify-no-senders
	:do-mach-notify-send-once
	:do-mach-notify-dead-name)

(defcfun ("set_notify_routine" %set-notify-routine) :void
  (what notify-routine-code)
  (fun :pointer))

; special setf form
(defun set-notify-routine (what fun)
  (declare (type symbol what))
  (%set-notify-routine what fun))

(defsetf notify-routine set-notify-routine)

; for debugging pruposes
(defcfun ("get_notify_info" %get-notify-info) :void)

(defcfun ("lisp_notify_server" %lisp-notify-server) :boolean
  (in :pointer)
  (out :pointer))

(defun notify-server (in out)
  (%lisp-notify-server in out))

(defmacro def-notify-interface (name params &body body)
  (with-gensyms (result)
	  `(define-hurd-interface notify-routine ,name ,params
		 ;(warn "got notify-routine~%")
		 (let ((,result (progn ,@body)))
		   (if (null ,result)
			 :operation-not-supported
			 ,result)))))
