;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:hurd-streams-asd
  (:use :cl :asdf))

(in-package :hurd-streams-asd)

(defsystem hurd-streams
  :name "hurd-streams"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "File streams using Hurd IO ports."
  :depends-on (:cffi
                :hurd-common
                :mach
                :hurd
                :trivial-gray-streams)
  :components ((:module streams
                        :components ((:file "package")
                                     (:file "stream"
                                            :depends-on ("package"))
                                     (:file "output"
                                            :depends-on ("stream"))
                                     (:file "input"
                                            :depends-on ("stream"))))))

