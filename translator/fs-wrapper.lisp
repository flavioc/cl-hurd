
(in-package :hurd-translator)

(define-stub-library fs)

;; All filesystem routines.
(defcenum fs-routine-code
  :FILE-EXEC
  :FILE-CHOWN
  :FILE-CHAUTHOR
  :FILE-CHMOD
  :FILE-CHFLAGS
  :FILE-UTIMES
  :FILE-SET-SIZE
  :FILE-LOCK
  :FILE-LOCK-STAT
  :FILE-CHECK-ACCESS
  :FILE-NOTICE-CHANGES
  :FILE-GETCONTROL
  :FILE-STATFS
  :FILE-SYNC
  :FILE-SYNCFS
  :FILE-GET-STORAGE-INFO
  :FILE-GETLINKNODE
  :FILE-GETFH
  :DIR-LOOKUP
  :DIR-READDIR
  :DIR-MKDIR
  :DIR-RMDIR
  :DIR-UNLINK
  :DIR-LINK
  :DIR-RENAME
  :DIR-MKFILE
  :DIR-NOTICE-CHANGES
  :FILE-SET-TRANSLATOR
  :FILE-GET-TRANSLATOR
  :FILE-GET-TRANSLATOR-CNTL
  :FILE-GET-FS-OPTIONS
  :FILE-REPARENT)

(defcfun ("set_fs_routine" %set-fs-routine) :void
   (what fs-routine-code)
   (fun :pointer))

(defun set-fs-routine (what fun)
  "Sets 'what' routine as the function 'fun'."
  (declare (type symbol what))
  (%set-fs-routine what fun))

(defsetf fs-routine set-fs-routine)

(defcfun ("get_fs_info" %get-fs-info) :void)

(defcfun ("lisp_fs_server" %lisp-fs-server)
  :boolean
  (in :pointer)
  (out :pointer))

(defun fs-server (in out)
  "Filesystem server."
  (%lisp-fs-server in out))
