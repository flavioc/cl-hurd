
(in-package :hurd)

(defun load-hurd-libraries ()
  (define-foreign-library liblisptrans
                          (t (:default "liblisptrans")))
  (use-foreign-library liblisptrans))

(load-hurd-libraries)

