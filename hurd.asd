;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:hurd-asd
  (:use :cl :asdf))

(in-package :hurd-asd)

(defsystem hurd
  :name "cl-hurd"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Common Lisp bindings for the Hurd"
  :depends-on (:cffi :trivial-garbage :hurd-common :mach)
  :components ((:file "paths")
               (:module hurd
                        :components ((:file "package")
                                     (:file "types"
                                            :depends-on ("package"))
                                     (:file "paths"
                                            :depends-on ("package"))
                                     (:file "retry"
                                            :depends-on ("package"))
                                     (:file "macros"
                                            :depends-on ("package"))
                                     (:file "translator-options"
                                            :depends-on ("package"))
                                     (:module libc
                                              :components ((:file "getcwdir")
                                                           (:file "file-name-lookup")
                                                           (:file "get-privileged-ports")
                                                           (:file "getauth")
                                                           (:file "getcrdir")
                                                           (:file "getcttyid")
                                                           (:file "geteuids")
                                                           (:file "getproc")
                                                           (:file "getumask")
                                                           (:file "pid2task")
                                                           (:file "setauth")
                                                           (:file "setcttyid")
                                                           (:file "setcwdir")
                                                           (:file "seteuids")
                                                           (:file "setproc")
                                                           (:file "task2pid"))
                                              :depends-on ("types" "package"))
                                     (:module io
                                              :components ((:file "server-version")
                                                           (:file "open-modes")
                                                           (:file "duplicate")
                                                           (:file "get-owner")
                                                           (:file "identity")
                                                           (:file "mod-owner")
                                                           (:file "pathconf")
                                                           (:file "read")
                                                           (:file "readable")
                                                           (:file "restrict-auth")
                                                           (:file "revoke")
                                                           (:file "seek")
                                                           (:file "select")
                                                           (:file "write")
                                                           (:file "stat")))
                                     (:module fs
                                              :components ((:file "trans-flags")
                                                           (:file "file-get-translator")
                                                           (:file "file-get-translator-cntl")
                                                           (:file "file-chown")
                                                           (:file "file-chauthor")
                                                           (:file "file-chmod")
                                                           (:file "file-utimes")
                                                           (:file "file-set-size")
                                                           (:file "file-lock")
                                                           (:file "file-lock-stat")
                                                           (:file "file-check-access")
                                                           (:file "file-getcontrol")
                                                           (:file "file-statfs")
                                                           (:file "file-sync")
                                                           (:file "file-syncfs")
                                                           (:file "file-getlinknode")
                                                           (:file "dir-lookup")
                                                           (:file "dir-readdir")
                                                           (:file "file-set-translator"
                                                                  :depends-on ("trans-flags"))
                                                           (:file "file-reparent")
                                                           (:file "file-get-fs-options")
                                                           (:file "dir-unlink")
                                                           (:file "dir-rmdir")
                                                           (:file "dir-rename")
                                                           (:file "dir-mkfile")
                                                           (:file "dir-mkdir")
                                                           (:file "dir-link")
                                                           (:file "storage"))
                                              :depends-on ("retry" "fsys" "translator-options" "package"))
                                     (:module iohelp
                                              :components ((:file "utils")
                                                           (:file "iouser"
                                                                  :depends-on ("utils"))
                                                           (:file "reauth"
                                                                  :depends-on ("iouser")))
                                              :depends-on ("types" "package"))
                                     (:module auth
                                              :components ((:file "getids"))
                                              :depends-on ("types" "iohelp" "package"))
                                     (:module exec
                                              :components ((:file "flags")
                                                           (:file "exec"
                                                                  :depends-on ("flags")))
                                              :depends-on ("types" "package"))
                                     (:module fsys
                                              :components ((:file "startup")
                                                           (:file "goaway-flags")
                                                           (:file "goaway-reply")
                                                           (:file "goaway"
                                                                  :depends-on ("goaway-flags"))
                                                           (:file "set-options")
                                                           (:file "syncfs")
                                                           (:file "get-options")
                                                           (:file "getroot"))
                                              :depends-on ("iohelp" "retry" "translator-options" "package"))
                                     (:module ports
                                              :components ((:file "port")
                                                           (:file "bucket"
                                                                  :depends-on ("port"))
                                                           (:file "notify")
                                                           (:file "no-senders"
                                                                  :depends-on ("bucket"
                                                                               "notify"))
                                                           (:file "dead-name"
                                                                  :depends-on ("bucket"
                                                                               "notify"))
                                                           (:file "demuxer"
                                                                  :depends-on ("notify"))
                                                           (:file "server"
                                                                  :depends-on ("port"
                                                                               "bucket"
                                                                               "no-senders"
                                                                               "notify"
                                                                               "demuxer")))
                                              :depends-on ("macros" "package"))
                                     (:module fshelp
                                              :components ((:file "access")
                                                           (:file "checkdirmod")
                                                           (:file "isowner")
                                                           (:file "iscontroller")
                                                           (:file "transbox")
                                                           (:file "fetch-root"
                                                                  :depends-on ("transbox"))
                                                           (:file "identity"))
                                              :depends-on ("iohelp" "fsys" "ports" "package")))
                        :depends-on ("paths"))))

