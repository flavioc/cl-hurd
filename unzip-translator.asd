;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:unzip-translator-asd
  (:use :cl :asdf))

(in-package :unzip-translator-asd)

(defsystem unzip-translator
  :name "unzip-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Simple, yet functional zip translator with read-only support."
  :depends-on (:zip :tree-translator :hurd-streams)
  :components ((:module unzip-translator
                        :components (:file "unzip-translator"))))

