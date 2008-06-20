
(in-package #:cl-user)

(defpackage :cl-hurd.common
  (:nicknames :hurd-common)
  (:use :cl :cffi)
  (:export :largest-representable-number
		   :num-bits
		   :define-helper-library
		   :define-stub-library
		   :error->string
		   :err
		   :unless-return
		   :translate-foreign-list
		   :with-gensyms
		   :select-error))

(defpackage :cl-mach
  (:nicknames :mach)
  (:use :cl :cffi :hurd-common)
  (:export :task-self
	   :with-port
	   :port
	   :port-valid
	   :port-allocate
	   :port-deallocate
	   :port-destroy
	   :port-mscount
	   :msg-seqno
	   :get-bootstrap-port))

(defpackage :cl-hurd
  (:nicknames :hurd)
  (:use :cl :cffi :mach :hurd-common :tg)
  (:export :getauth
		   :ports-create-class
		   :ports-create-bucket
		   :create-port
		   :get-send-right
		   ))

(defpackage :cl-hurd.translator
  (:nicknames :hurd-translator)
  (:use :cl :cffi :mach :hurd-common :hurd :tg))
