;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:proxy-translator-asd
  (:use :cl :asdf))

(in-package :proxy-translator-asd)

(defsystem proxy-translator
  :name "proxy-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "A proxy translator."
  :depends-on (:hurd)
  :components ((:file "proxy")))
