
(in-package :hurd)

(defcfun ("io_restrict_auth" %io-restrict-auth)
  err
  (file port)
  (new-object port-pointer)
  (uids :pointer)
  (uids-len msg-type-number)
  (gids :pointer)
  (gids-len msg-type-number))

(defun io-restrict-auth (file user)
  "Return a new port restricted to iouser 'user'."
  (declare (type fixnum file)
           (type iouser user))
  (let* ((uids-l (get-foreign-uids user))
         (uids-ptr (first uids-l))
         (uids-len (second uids-l))
         (gids-l (get-foreign-gids user))
         (gids-ptr (first gids-l))
         (gids-len (second gids-l)))
    (with-cleanup (free-memory-list (list uids-ptr gids-ptr))
      (with-foreign-pointer (new-object (foreign-type-size 'port))
        (select-error (%io-restrict-auth file
                                         new-object
                                         uids-ptr
                                         uids-len
                                         gids-ptr
                                         gids-len)
                      (mem-ref new-object 'port))))))

