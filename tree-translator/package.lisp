
(in-package #:cl-user)

(defpackage :cl-hurd.translator.tree
  (:nicknames :hurd-tree-translator)
  (:use :cl :hurd-common :mach :hurd :hurd-translator)
  (:export :fill-root-node
           :tree-translator
           :dir-entry
           :dir-size
           :entry
           :add-entry
           :get-entry
           :rename-dir-entry
           :remove-dir-entry
           :setup-entry
           :iterate-entries
           :iterate-entries-deep
           :has-entry-p
           :get-dir-entries
           :parent
           :clear-dir))

