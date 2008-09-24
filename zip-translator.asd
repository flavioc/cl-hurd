;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:zip-translator-asd
  (:use :cl :asdf))

(in-package :zip-translator-asd)

(defsystem zip-translator
  :name "zip-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Simple, yet functional zip translator."
  :depends-on (:zip :hurd-translator)
  :components ((:module zip-translator
                        :components ((:file "zip-translator")))))

