;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:translator-test-asd
  (:use :cl :asdf))

(in-package :translator-test-asd)

(defsystem translator-test
  :name "translator-test"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Translator test package."
  :depends-on (:hurd :xlunit)
  :components ((:file "package")
               (:file "macros"
                      :depends-on ("package"))
               (:file "paths"
                      :depends-on ("package"))
               (:file "io-suite"
                      :depends-on ("package" "paths" "macros"))
               (:file "io-server-version"
                      :depends-on ("io-suite"))
               (:file "io-pathconf"
                      :depends-on ("io-suite"))
               (:file "io-owner"
                      :depends-on ("io-suite"))
               (:file "io-duplicate"
                      :depends-on ("io-suite"))
               (:file "io-identity"
                      :depends-on ("io-suite"))
               (:file "io-openmodes"
                      :depends-on ("io-suite"))
               (:file "fs-suite"
                      :depends-on ("package" "paths" "macros"))
               (:file "file-check-access"
                      :depends-on ("fs-suite"))
               (:file "file-chown"
                      :depends-on ("fs-suite"))
               (:file "file-chauthor"
                      :depends-on ("fs-suite"))
               (:file "file-sync"
                      :depends-on ("fs-suite"))
               (:file "file-statfs"
                      :depends-on ("fs-suite"))
               (:file "file-utimes"
                      :depends-on ("fs-suite"))
               (:file "file-reparent"
                      :depends-on ("fs-suite"))
               (:file "file-getlinknode"
                      :depends-on ("fs-suite"))
               (:file "fsys-suite"
                      :depends-on ("package" "macros" "paths"))
               (:file "fsys-options"
                      :depends-on ("fsys-suite"))))

