
(in-package :hurd-translator)

(defcenum io-routine-code
  :IO-WRITE
  :IO-READ
  :IO-SEEK
  :IO-READABLE
  :IO-SET-ALL-OPENMODES
  :IO-GET-OPENMODES
  :IO-SET-SOME-OPENMODES
  :IO-CLEAR-SOME-OPENMODES
  :IO-ASYNC
  :IO-MOD-OWNER
  :IO-GET-OWNER
  :IO-GET-ICKY-ASYNC-ID
  :IO-SELECT
  :IO-STAT
  :IO-REAUTHENTICATE
  :IO-RESTRICT-AUTH
  :IO-DUPLICATE
  :IO-SERVER-VERSION
  :IO-MAP
  :IO-MAP-CNTL
  :IO-GET-CONCH
  :IO-RELEASE-CONCH
  :IO-EOFNOTIFY
  :IO-PRENOTIFY
  :IO-POSTNOTIFY
  :IO-READNOTIFY
  :IO-READSLEEP
  :IO-SIGIO
  :IO-PATHCONF
  :IO-IDENTITY
  :IO-REVOKE)

(defcfun ("set_io_routine" %set-io-routine) :void
  (what io-routine-code)
  (fun :pointer))

; special setf form
(defun set-io-routine (what fun)
  "Sets the 'what' routine as the 'fun' function."
  (declare (type symbol what))
  (%set-io-routine what fun))

(defsetf io-routine set-io-routine)

(defcfun ("get_io_info" %get-io-info) :void)

(defcfun ("lisp_S_io_server" %lisp-io-server) :boolean
  (in :pointer)
  (out :pointer))

(defun io-server (in out)
  "The IO server."
  (%lisp-io-server in out))
