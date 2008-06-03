
(in-package :mach)

(define-foreign-library libmachuser
	(:unix (:or "libmachuser-2.7.so" "libmachuser.so.1"))
	(t (:default "libmachuser")))

(use-foreign-library libmachuser)
	
(load "mach/types")
(load "mach/error")
(load "mach/functions")
