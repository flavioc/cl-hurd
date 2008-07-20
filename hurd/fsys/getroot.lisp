
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
  (flags open-flags)
  (do-retry :pointer)
  (retry-name :pointer)
  (file port-pointer))

(defun fsys-getroot (control dotdot dotdot-poly user flags)
  "Returns a file to the root of the filesystem. See hurd/fsys.defs for details."
  (with-foreign-pointer (root (foreign-type-size 'port))
    (with-foreign-pointer (retry-name 2048)
      (with-foreign-pointer (do-retry (foreign-type-size 'retry-type))
        (let ((uids (get-foreign-uids user))
              (gids (get-foreign-gids user)))
          (with-cleanup (free-memory-list (list (first uids) (first gids)))
            (let ((return-code (%fsys-getroot control
                                              dotdot
                                              dotdot-poly
                                              (first uids) (second uids) ; pointer and size
                                              (first gids) (second gids) ; pointer and size
                                              flags
                                              do-retry
                                              retry-name
                                              root)))
              (select-error return-code (values
                                          (mem-ref do-retry 'retry-type)
                                          (foreign-string-to-lisp retry-name)
                                          (mem-ref root 'port))))))))))
