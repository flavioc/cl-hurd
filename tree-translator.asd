;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:tree-translator-asd
  (:use :cl :asdf))

(in-package :tree-translator-asd)

(defsystem tree-translator
  :name "tree-translator"
  :version "0.0.0"
  :maintainer "Flavio Cruz"
  :author "Flavio Cruz"
  :license "GPL v3.0"
  :description "Specialized tree translator."
  :depends-on (:cffi :hurd-common :mach
                     :hurd :hurd-translator)
  :components ((:module tree-translator
                        :components ((:file "package")
                                     (:file "sorted-container"
                                            :depends-on ("package"))
                                     (:file "dir"
                                            :depends-on ("sorted-container"))
                                     (:file "class"
                                            :depends-on ("dir"))))))

