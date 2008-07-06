
(in-package :hurd)

(defcfun ("fsys_getroot" %fsys-getroot)
  err
  (control port)
  (dotdot port)
  (dotdot-poly msg-type-name)
  (uids :pointer)
  (uids-count msg-type-number)
  (gids :pointer)
  (gids-count msg-type-number)
  (flags :int)
  (do-retry :pointer)
  (retry-name :string)
  (file port-pointer))

(defun fsys-getroot (control dotdot dotdot-poly
                             uids uids-count
                             gids gids-count
                             flags
                             do-retry retry-name)
  "Returns a file to tyhe root of the filesystem. See hurd/fsys.defs for details."
  (with-foreign-pointer (control (foreign-type-size 'pointer))
    (let ((return-code (%fsys-getroot control dotdot
                                      dotdot-poly
                                      uids uids-count
                                      gids gids-count
                                      flags do-retry
                                      retry-name control)))
      (select-error return-code (mem-ref control 'port)))))
