
(in-package #:cl-user)

(defpackage :cl-hurd.streams
  (:nicknames :hurd-streams)
  (:use :cl :hurd-common :mach :hurd :trivial-gray-streams)
  (:export :hurd-stream
           :hurd-input-stream
           :make-hurd-input-stream
           :with-hurd-input-stream))

