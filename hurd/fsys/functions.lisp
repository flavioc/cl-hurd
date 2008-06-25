
(defcfun ("fsys_startup" %fsys-startup)
		 err
		 (bootstrap port)
		 (flags open-flags)
		 (control-port port)
		 (control-poly msg-type-name)
		 (realnode port-pointer))

(defun fsys-startup (bootstrap flags send-right type)
  (with-foreign-pointer (port (foreign-type-size 'port))
    (let ((err (%fsys-startup
		 bootstrap
		 flags
		 send-right
		 type
		 port)))
      (select-error err (mem-ref port 'port)))))
