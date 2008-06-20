
(define-stub-library fsys)

(defcenum fsys-routine-code
	:FSYS-STARTUP
	:FSYS-GOAWAY
	:FSYS-GETROOT
	:FSYS-GETFILE
	:FSYS-SYNCFS
	:FSYS-SET-OPTIONS
	:FSYS-GETPRIV
	:FSYS-INIT
	:FSYS-FORWARD
	:FSYS-GET_OPTIONS)

(defcfun ("set_fsys_routine" %set-fsys-routine) :void
  (what fsys-routine-code)
  (fun :pointer))

(defun set-fsys-routine (what fun)
  (declare (type symbol what))
  (%set-fsys-routine what fun))

(defsetf fsys-routine set-fsys-routine)

(defcfun ("get_fsys_info" %get-fsys-info) :void)

;; server routine

(defcfun ("lisp_fsys_server" %lisp-fsys-server)
		 :boolean
		 (in :pointer)
		 (out :pointer))

(defun fsys-server (in out)
  (%lisp-fsys-server in out))
