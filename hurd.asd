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
                                     (:file "dirent")
                                     (:file "pathconf")
                                     (:file "types")
                                     (:file "ids"
                                            :depends-on ("types"))
                                     (:file "flags"
                                            :depends-on ("utils"))
                                     (:file "mode"
                                            :depends-on ("utils"))
                                     (:file "memcpy")
                                     (:file "stat"
                                            :depends-on ("mode"
                                                         "types"
                                                         "ids"
                                                         "memcpy"))
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
                                     (:file "msg-id")
                                     (:file "msg-option")
                                     (:file "types"
                                            :depends-on ("mach"
                                                         "port-type"
                                                         "msg-type-name"
                                                         "port-right"
                                                         "port"
                                                         "msg-id"
                                                         "msg-option"
                                                         "task-special-ports"))
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
                                     (:file "functions"
                                            :depends-on ("types"
                                                         "port-creation"
                                                         "port-destruction"
                                                         "port-rights"
                                                         "port-move"
                                                         "port-sets"
                                                         "port-request-notifications"
                                                         "msg-server"
                                                         "port-receive-rights"
                                                         "port-names"))
                                     (:file "macros"
                                            :depends-on ("functions"))
                                     (:file "maptime"
                                            :depends-on ("types"))
                                     (:file "mmap"
                                            :depends-on ("types"))
                                     (:file "round-page"
                                            :depends-on ("types"))
                                     (:file "vm-allocate"
                                            :depends-on ("types")))
                        :depends-on ("packages" "common" "paths"))
               (:module hurd
                        :components ((:file "types")
                                     (:file "paths")
                                     (:file "retry")
                                     (:file "fs-type")
                                     (:file "functions"
                                            :depends-on ("types"))
                                     (:file "macros")
                                     (:module io
                                              :components ((:file "server-version")
                                                           (:file "stat")))
                                     (:module iohelp
                                              :components ((:file "utils")
                                                           (:file "iouser"
                                                                  :depends-on ("utils"))))
                                     (:module fsys
                                              :components ((:file "startup")
                                                           (:file "getroot")))
                                     (:module fshelp
                                              :components ((:file "access")
                                                           (:file "checkdirmod")
                                                           (:file "isowner")
                                                           (:file "iscontroller")
                                                           (:file "fshelp")
                                                           (:file "touch"
                                                                  :depends-on ("fshelp"))
                                                           (:file "transbox")
                                                           (:file "fetch-root"))
                                              :depends-on ("iohelp" "fsys"))
                                     (:module ports
                                              :components ((:file "port")
                                                           (:file "bucket"
                                                                  :depends-on ("port"))
                                                           (:file "notify")
                                                           (:file "no-senders"
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
                                              :depends-on ("macros")))
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
                                     (:file "node")
                                     (:file "dirent"
                                            :depends-on ("node"))
                                     (:file "open"
                                            :depends-on ("node"))
                                     (:file "protid"
                                            :depends-on ("open"))
                                     (:file "class"
                                            :depends-on ("debug"
                                                         "node"
                                                         "open"
                                                         "protid"))
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
                                                           (:file "dir-lookup")
                                                           (:file "dir-readdir")
                                                           (:file "dir-rmdir")
                                                           (:file "file-chauthor")
                                                           (:file "file-chmod")
                                                           (:file "file-chown")
                                                           (:file "file-getlinknode")
                                                           (:file "file-sync")
                                                           (:file "file-syncfs")
                                                           (:file "file-utimes")
                                                           (:file "fsys-getroot")
                                                           (:file "io-clear-some-openmodes")
                                                           (:file "io-duplicate")
                                                           (:file "io-get-openmodes")
                                                           (:file "io-get-owner")
                                                           (:file "io-mod-owner")
                                                           (:file "io-pathconf")
                                                           (:file "io-read")
                                                           (:file "io-readable")
                                                           (:file "io-server-version")
                                                           (:file "io-set-all-openmodes")
                                                           (:file "io-set-some-openmodes")
                                                           (:file "io-stat")
                                                           (:file "io-write"))
                                              :depends-on ("io-wrapper" "fs-wrapper" "fsys-wrapper" "macros" "class" "api" "run" "globals")))
                        :depends-on ("packages" "paths" "common" "mach" "hurd"))
               (:module tree-translator
                        :components ((:file "sorted-container")
                                     (:file "dir"
                                            :depends-on ("sorted-container"))
                                     (:file "class"
                                            :depends-on ("dir")))
                        :depends-on ("packages" "paths" "common" "mach" "hurd" "translator"))
               (:module examples
                        :components ((:file "zip"))
                        :depends-on ("packages" "paths" "common" "mach" "hurd" "translator" "tree-translator"))))
