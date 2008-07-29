;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:hurd-common-asd
  (:use :cl :asdf))

(in-package :hurd-common-asd)

(defsystem hurd-common
  :name "cl-hurd-common"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Support system for the Common Lisp hurd bindings."
  :depends-on (:cffi :trivial-garbage)
  :components ((:module common
                        :components ((:file "package")
                                     (:file "utils"
                                            :depends-on ("package"))
                                     (:file "constants")
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
                                            :depends-on ("error"))))))

