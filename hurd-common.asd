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
                                     (:file "constants"
                                            :depends-on ("package"))
                                     (:file "error"
                                            :depends-on ("utils"))
                                     (:file "types"
                                            :depends-on ("package"))
                                     (:file "dirent"
                                            :depends-on ("types" "package"))
                                     (:file "pathconf"
                                            :depends-on ("package"))
                                     (:file "seek"
                                            :depends-on ("package"))
                                     (:file "select"
                                            :depends-on ("package"))
                                     (:file "exit"
                                            :depends-on ("package"))
                                     (:file "lock"
                                            :depends-on ("package"))
                                     (:file "ids"
                                            :depends-on ("types"))
                                     (:file "open-flags"
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
                                     (:file "device-id"
                                            :depends-on ("package"))
                                     (:file "memcmp"
                                            :depends-on ("package"))
                                     (:file "stat"
                                            :depends-on ("mode"
                                                         "types"
                                                         "memcmp"
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

