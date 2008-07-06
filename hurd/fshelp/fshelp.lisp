
(in-package :hurd)

;; Load libfshelp library.

(define-foreign-library libfshelp
	(:unix (:or "libfshelp.so.0.3"))
	(t (:default "libfshelp")))

(use-foreign-library libfshelp)

