
(in-package :hurd)

(define-stub-library notify)

;; Types of callbacks for a notify server.
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

(defun set-notify-routine (what fun)
  "Sets a function to be run on 'what' events."
  (declare (type symbol what))
  (%set-notify-routine what fun))

(defsetf notify-routine set-notify-routine)

;; For debugging purposes
(defcfun ("get_notify_info" %get-notify-info) :void)

(defcfun ("lisp_notify_server" %lisp-notify-server) :boolean
  (in :pointer)
  (out :pointer))

(defun notify-server (in out)
  "Notify server."
  (%lisp-notify-server in out))

(defmacro def-notify-interface (name params &body body)
  "Defines a new notify callback."
  (with-gensyms (result)
	  `(define-hurd-interface notify-routine ,name ,params
       (let ((,result (progn ,@body)))
         (if (null ,result)
           :operation-not-supported
           ,result)))))
