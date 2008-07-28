;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:tmp-translator-asd
  (:use :cl :asdf))

(in-package :tmp-translator-asd)

(defsystem tmp-translator
  :name "tmp-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Simple tmpfs like translator."
  :depends-on (:hurd)
  :components ((:file "tmp")))
