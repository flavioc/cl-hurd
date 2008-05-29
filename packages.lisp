
(in-package #:cl-user)

(defpackage :cl-hurd.common
  (:nicknames :hurd-common)
  (:use :cl :cffi)
  (:export :largest-representable-number
		   :num-bits
		   :define-helper-library))

(defpackage :cl-hurd.error
  (:nicknames :hurd-error)
  (:use :cl :cffi :hurd-common))

(defpackage :cl-mach
  (:nicknames :mach)
  (:use :cl :cffi :hurd-common)
  (:export :task-self))

(defpackage :cl-hurd
  (:nicknames :hurd)
  (:use :cl :cffi :mach :hurd-common))

