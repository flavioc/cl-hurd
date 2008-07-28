;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:mod-translator-asd
  (:use :cl :asdf))

(in-package :mod-translator-asd)

(defsystem mod-translator
  :name "mod-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Translator that watches for changes in the underlying node."
  :depends-on (:hurd :ext)
  :components ((:file "mod")))
