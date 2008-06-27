
(in-package :hurd)

(define-foreign-library libfshelp
	(:unix (:or "libfshelp.so.0.3"))
	(t (:default "libfshelp")))
(use-foreign-library libfshelp)

(load "hurd/types")
(load "hurd/functions")
(load "hurd/ports/ports")
(load "hurd/fsys/fsys")
(load "hurd/io/io")
(load "hurd/iohelp/iohelp")
(load "hurd/paths")
