;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:mux-translator-asd
  (:use :cl :asdf))

(in-package :mux-translator-asd)

(defsystem mux-translator
  :name "mux-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "A multiplexer translator."
  :depends-on (:tree-translator :split-sequence :trivial-garbage)
  :components ((:file "mux")))
