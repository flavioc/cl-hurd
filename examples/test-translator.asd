;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:test-translator-asd
  (:use :cl :asdf))

(in-package :test-translator-asd)

(defsystem test-translator
  :name "test-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Translator used for the tests."
  :depends-on (:tree-translator)
  :components ((:file "test")))
