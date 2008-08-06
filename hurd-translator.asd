;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:hurd-translator-asd
  (:use :cl :asdf))

(in-package :hurd-translator-asd)

(defsystem hurd-translator
  :name "cl-hurd-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Common Lisp translator library for the Hurd"
  :depends-on (:cffi :flexi-streams :trivial-garbage
                     :hurd-common :mach :hurd)
  :components ((:module translator
                        :components ((:file "package")
                                     (:file "io-wrapper"
                                            :depends-on ("package"))
                                     (:file "fs-wrapper"
                                            :depends-on ("package"))
                                     (:file "fsys-wrapper"
                                            :depends-on ("package"))
                                     (:file "debug"
                                            :depends-on ("io-wrapper"
                                                         "fs-wrapper"
                                                         "fsys-wrapper"))
                                     (:file "globals"
                                            :depends-on ("package"))
                                     (:file "transbox"
                                            :depends-on ("package"))
                                     (:file "node"
                                            :depends-on ("package"))
                                     (:file "dirent"
                                            :depends-on ("node"))
                                     (:file "open"
                                            :depends-on ("node"))
                                     (:file "protid"
                                            :depends-on ("open"
                                                         "globals"))
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
                                              :depends-on ("io-wrapper" "fs-wrapper" "fsys-wrapper" "macros" "class" "api" "run" "globals" "dirent" "utils" "package"))))))
