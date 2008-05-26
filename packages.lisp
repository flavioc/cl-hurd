
(in-package #:cl-user)

(defpackage :cl-mach
  (:nicknames :mach)
  (:use :cl :cffi))

(defpackage :cl-hurd
  (:nicknames :hurd)
  (:use :cl :cffi :mach))

