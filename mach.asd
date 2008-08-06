;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:mach-asd
  (:use :cl :asdf))

(in-package :mach-asd)

(defsystem mach
  :name "cl-mach"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Common Lisp bindings for the GNU Mach"
  :depends-on (:cffi :trivial-garbage :hurd-common)
  :components ((:module mach
                        :components ((:file "package")
                                     (:file "msg-type-name"
                                            :depends-on ("package"))
                                     (:file "task-special-ports"
                                            :depends-on ("package"))
                                     (:file "port-right"
                                            :depends-on ("package"))
                                     (:file "port-type"
                                            :depends-on ("package"
                                                         "port-right"))
                                     (:file "port")
                                     (:file "msg-notify")
                                     (:file "msg-option")
                                     (:file "msg-type")
                                     (:file "types"
                                            :depends-on ("port-type"
                                                         "package"
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
                                            :depends-on ("types"
                                                         "package"))
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
                                     (:file "vm-allocate"
                                            :depends-on ("types"))))))
