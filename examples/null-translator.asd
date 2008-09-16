;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:null-translator-asd
  (:use :cl :asdf))

(in-package :null-translator-asd)

(defsystem null-translator
  :name "null-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "/dev/null translator."
  :depends-on (:hurd-translator)
  :components ((:file "null-translator")))

