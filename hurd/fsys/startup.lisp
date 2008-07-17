
(in-package :hurd)

(defcfun ("fsys_startup" %fsys-startup)
  err
  (bootstrap port)
  (flags open-flags)
  (control-port port)
  (control-poly msg-type-name)
  (realnode port-pointer))

(defun fsys-startup (bootstrap flags send-right type)
  "This is sent by filesystem on its bootstrap port upon startup. Returns the underlying node port."
  (with-foreign-pointer (port (foreign-type-size 'port))
    (let ((err (%fsys-startup
                 bootstrap
                 flags
                 send-right
                 type
                 port)))
      (select-error err (mem-ref port 'port)))))

