
(in-package :hurd)

(define-foreign-library libfshelp
	(:unix (:or "libfshelp.so.0.3"))
	(t (:default "libfshelp")))
(use-foreign-library libfshelp)

(define-foreign-library libports
	(t (:default "libports")))
(use-foreign-library libfshelp)

(load "hurd/types")
(load "hurd/functions")
(load "hurd/ports/ports")
(load "hurd/fsys/fsys")
