
(in-package :hurd)

(define-foreign-library libhurduser
	(:unix (:or "libhurduser.so.0.3"))
	(t (:default "libhurduser")))

(use-foreign-library libhurduser)
