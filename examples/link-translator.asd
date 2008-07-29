;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:link-translator-asd
  (:use :cl :asdf))

(in-package :link-translator-asd)

(defsystem link-translator
  :name "link-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Translator that creates a simple link node."
  :depends-on (:hurd-translator)
  :components ((:file "link")))
