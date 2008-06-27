
(in-package #:cl-user)

(defpackage :cl-hurd.common
  (:nicknames :hurd-common)
  (:use :cl :cffi :tg)
  (:export :largest-representable-number
		   :num-bits
		   :define-helper-library
		   :define-stub-library
		   :error->string
		   :err
		   :unless-return
		   :translate-foreign-list
		   :with-gensyms
		   :select-error
		   :open-flags-t
		   :mode-t
		   :open-flags
		   :with-cleanup))

(defpackage :cl-mach
  (:nicknames :mach)
  (:use :cl :cffi :hurd-common)
  (:export :task-self
	   :with-port-deallocate
	   :with-port-destroy
	   :with-port
	   :with-send-right
	   :with-receive-right
	   :port
	   :is
	   :port-pointer
	   :port-valid
	   :port-allocate
	   :port-deallocate
	   :port-destroy
	   :port-mod-refs
	   :port-move-member
	   :port-insert-right
	   :port-type
	   :port-request-notification
	   :port-mscount
	   :msg-seqno
	   :get-bootstrap-port
	   :msg-type-name
	   :msg-server-timeout))

(defpackage :cl-hurd
  (:nicknames :hurd)
  (:use :cl :cffi :mach :hurd-common :tg)
  (:export :getauth
		   :get-send-right
		   :fsys-startup
		   :make-bucket
		   :run-server
		   :add-port
		   :has-port
		   :+servers-exec+
		   :file-name-lookup
		   ))

(defpackage :cl-hurd.translator
  (:nicknames :hurd-translator)
  (:use :cl :cffi :mach :hurd-common :hurd :tg))

