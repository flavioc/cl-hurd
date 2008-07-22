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
  :depends-on (:cffi :flexi-streams :zip :trivial-garbage)
  :components ((:file "packages")
               (:file "paths")
               (:module common
                        :components ((:file "utils")
                                     (:file "error"
                                            :depends-on ("utils"))
                                     (:file "types")
                                     (:file "dirent"
                                            :depends-on ("types"))
                                     (:file "pathconf")
                                     (:file "seek")
                                     (:file "select")
                                     (:file "exit")
                                     (:file "lock")
                                     (:file "ids"
                                            :depends-on ("types"))
                                     (:file "flags"
                                            :depends-on ("utils"))
                                     (:file "fs-type")
                                     (:file "mode"
                                            :depends-on ("utils"))
                                     (:file "memcpy")
                                     (:file "maptime"
                                            :depends-on ("types"
                                                         "error"))
                                     (:file "time-value"
                                            :depends-on ("types"
                                                         "maptime"))
                                     (:file "device-id")
                                     (:file "stat"
                                            :depends-on ("mode"
                                                         "types"
                                                         "ids"
                                                         "memcpy"
                                                         "time-value"
                                                         "device-id"
                                                         "maptime"))
                                     (:file "statfs"
                                            :depends-on ("types"
                                                         "fs-type"))
                                     (:file "functions"
                                            :depends-on ("error")))
                        :depends-on ("packages" "paths"))
               (:module mach
                        :components ((:file "mach")
                                     (:file "msg-type-name")
                                     (:file "task-special-ports")
                                     (:file "port-right")
                                     (:file "port-type"
                                            :depends-on ("port-right"))
                                     (:file "port")
                                     (:file "msg-notify")
                                     (:file "msg-option")
                                     (:file "msg-type")
                                     (:file "types"
                                            :depends-on ("mach"
                                                         "port-type"
                                                         "msg-type-name"
                                                         "port-right"
                                                         "port"
                                                         "msg-notify"
                                                         "msg-option"
                                                         "msg-type"
                                                         "task-special-ports"))
                                     (:file "msg-header"
                                            :depends-on ("types"))
                                     (:file "port-creation"
                                            :depends-on ("types"))
                                     (:file "port-destruction"
                                            :depends-on ("types"))
                                     (:file "task"
                                            :depends-on ("types"))
                                     (:file "port-names"
                                            :depends-on ("types"
                                                         "mmap"
                                                         "task"))
                                     (:file "port-rights"
                                            :depends-on ("types"
                                                         "task"))
                                     (:file "port-move"
                                            :depends-on ("types"
                                                         "task"))
                                     (:file "port-status"
                                            :depends-on ("types"))
                                     (:file "port-receive-rights"
                                            :depends-on ("types"
                                                         "port-status"
                                                         "task"))
                                     (:file "port-sets"
                                            :depends-on ("types"
                                                         "task"))
                                     (:file "port-request-notifications"
                                            :depends-on ("types"
                                                         "task"))
                                     (:file "msg-server"
                                            :depends-on ("types"
                                                         "task"))
                                     (:file "message"
                                            :depends-on ("types"
                                                         "msg-header"
                                                         "port-creation"))
                                     (:file "functions"
                                            :depends-on ("types"
                                                         "port-creation"
                                                         "port-destruction"
                                                         "port-rights"
                                                         "port-move"
                                                         "message"
                                                         "port-sets"
                                                         "port-request-notifications"
                                                         "msg-server"
                                                         "port-receive-rights"
                                                         "port-names"))
                                     (:file "macros"
                                            :depends-on ("functions"))
                                     (:file "mmap"
                                            :depends-on ("types"))
                                     (:file "round-page"
                                            :depends-on ("types"))
                                     (:file "vm-allocate"
                                            :depends-on ("types")))
                        :depends-on ("packages" "common" "paths"))
               (:module hurd
                        :components ((:file "libs")
                                     (:file "types")
                                     (:file "paths")
                                     (:file "retry")
                                     (:file "functions"
                                            :depends-on ("types"))
                                     (:file "macros")
                                     (:module io
                                              :components ((:file "server-version")
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
                                                           (:file "storage")))
                                     (:module iohelp
                                              :components ((:file "utils")
                                                           (:file "iouser"
                                                                  :depends-on ("utils"))
                                                           (:file "reauth"
                                                                  :depends-on ("iouser")))
                                              :depends-on ("libs" "types"))
                                     (:module auth
                                              :components ((:file "getids"))
                                              :depends-on ("types" "libs" "iohelp"))
                                     (:module exec
                                              :components ((:file "flags")
                                                           (:file "exec"
                                                                  :depends-on ("flags")))
                                              :depends-on ("types" "libs"))
                                     (:module fsys
                                              :components ((:file "startup")
                                                           (:file "goaway-flags")
                                                           (:file "goaway-reply")
                                                           (:file "goaway"
                                                                  :depends-on ("goaway-flags"))
                                                           (:file "getroot"))
                                              :depends-on ("iohelp" "retry"))
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
                                              :depends-on ("macros"))
                                     (:module fshelp
                                              :components ((:file "access")
                                                           (:file "checkdirmod")
                                                           (:file "isowner")
                                                           (:file "iscontroller")
                                                           (:file "fshelp")
                                                           (:file "touch"
                                                                  :depends-on ("fshelp"))
                                                           (:file "transbox")
                                                           (:file "fetch-root"
                                                                  :depends-on ("transbox"))
                                                           (:file "identity"))
                                              :depends-on ("iohelp" "fsys" "ports")))
                        :depends-on ("packages" "paths" "common" "mach"))
               (:module translator
                        :components ((:file "io-wrapper")
                                     (:file "fs-wrapper")
                                     (:file "fsys-wrapper")
                                     (:file "debug"
                                            :depends-on ("io-wrapper"
                                                         "fs-wrapper"
                                                         "fsys-wrapper"))
                                     (:file "globals")
                                     (:file "transbox")
                                     (:file "node")
                                     (:file "dirent"
                                            :depends-on ("node"))
                                     (:file "open"
                                            :depends-on ("node"))
                                     (:file "protid"
                                            :depends-on ("open"
                                                         "globals"))
                                     (:file "options")
                                     (:file "class"
                                            :depends-on ("debug"
                                                         "node"
                                                         "open"
                                                         "protid"
                                                         "options"))
                                     (:file "api"
                                            :depends-on ("class"))
                                     (:file "macros")
                                     (:file "demuxer"
                                            :depends-on ("io-wrapper"
                                                         "fs-wrapper"
                                                         "fsys-wrapper"
                                                         "macros"))
                                     (:file "run"
                                            :depends-on ("api"
                                                         "demuxer"
                                                         "class"))
                                     (:file "utils"
                                            :depends-on ("class"
                                                         "globals"))
                                     (:module interfaces
                                              :components ((:file "common")
                                                           (:file "dir-mkdir")
                                                           (:file "open-modes")
                                                           (:file "getroot-lookup-callbacks")
                                                           (:file "dir-lookup"
                                                                  :depends-on ("open-modes"
                                                                               "getroot-lookup-callbacks"))
                                                           (:file "dir-readdir")
                                                           (:file "dir-rmdir")
                                                           (:file "file-chauthor")
                                                           (:file "file-chmod")
                                                           (:file "file-chown")
                                                           (:file "file-getlinknode")
                                                           (:file "file-sync")
                                                           (:file "file-syncfs")
                                                           (:file "file-utimes")
                                                           (:file "fsys-getroot"
                                                                  :depends-on ("open-modes"
                                                                               "getroot-lookup-callbacks"))
                                                           (:file "io-clear-some-openmodes")
                                                           (:file "io-duplicate")
                                                           (:file "io-get-openmodes")
                                                           (:file "io-get-owner")
                                                           (:file "io-mod-owner")
                                                           (:file "io-pathconf")
                                                           (:file "io-read"
                                                                  :depends-on ("common"))
                                                           (:file "io-readable")
                                                           (:file "io-server-version")
                                                           (:file "io-set-all-openmodes")
                                                           (:file "io-set-some-openmodes")
                                                           (:file "io-stat")
                                                           (:file "io-write"
                                                                  :depends-on ("common"))
                                                           (:file "fsys-goaway")
                                                           (:file "file-getcontrol")
                                                           (:file "fsys-syncfs")
                                                           (:file "file-statfs")
                                                           (:file "file-check-access")
                                                           (:file "io-seek")
                                                           (:file "file-set-size")
                                                           (:file "dir-rename")
                                                           (:file "io-reauthenticate")
                                                           (:file "io-restrict-auth")
                                                           (:file "io-revoke")
                                                           (:file "io-identity")
                                                           (:file "dir-unlink")
                                                           (:file "dir-mkfile"
                                                                  :depends-on ("open-modes"))
                                                           (:file "dir-link")
                                                           (:file "io-select")
                                                           (:file "file-get-translator-cntl")
                                                           (:file "options")
                                                           (:file "fsys-get-options"
                                                                  :depends-on ("options"))
                                                           (:file "file-get-fs-options"
                                                                  :depends-on ("options"))
                                                           (:file "fsys-set-options")
                                                           (:file "file-exec")
                                                           (:file "file-set-translator")
                                                           (:file "file-get-translator")
                                                           (:file "file-reparent")
                                                           (:file "file-lock-stat")
                                                           (:file "file-get-storage-info"))
                                              :depends-on ("io-wrapper" "fs-wrapper" "fsys-wrapper" "macros" "class" "api" "run" "globals" "dirent" "utils")))
                        :depends-on ("packages" "paths" "common" "mach" "hurd"))
               (:module tree-translator
                        :components ((:file "sorted-container")
                                     (:file "dir"
                                            :depends-on ("sorted-container"))
                                     (:file "class"
                                            :depends-on ("dir")))
                        :depends-on ("packages" "paths" "common" "mach" "hurd" "translator"))
               (:module examples
                        :components (
                                     (:file "zip")
                                     ;(:file "link")
                                     )
                        :depends-on ("packages" "paths" "common" "mach" "hurd" "translator" "tree-translator"))))
