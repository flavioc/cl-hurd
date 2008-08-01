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
               (:file "paths"
                      :depends-on ("package"))
               (:file "io-suite"
                      :depends-on ("package" "paths"))
               (:file "io-server-version"
                      :depends-on ("io-suite"))
               (:file "io-pathconf"
                      :depends-on ("io-suite"))))

