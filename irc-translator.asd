;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:irc-translator-asd
  (:use :cl :asdf))

(in-package :irc-translator-asd)

(defsystem irc-translator
  :name "irc-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Irc client implemented as a filesystem."
  :depends-on (:cl-irc :tree-translator)
  :components ((:module irc-translator
                        :components ((:file "irc-translator")))))

