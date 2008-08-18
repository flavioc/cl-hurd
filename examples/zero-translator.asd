;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:zero-translator-asd
  (:use :cl :asdf))

(in-package :zero-translator-asd)

(defsystem zero-translator
  :name "zero-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "/dev/zero translator."
  :depends-on (:hurd-translator)
  :components ((:file "zero")))

