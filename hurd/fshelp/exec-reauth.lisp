
(in-package :hurd)

(defcenum init-port
  :init-port-cwdir
  :init-port-crdir
  :init-port-auth
  :init-port-proc
  :init-port-cttyid
  :init-port-bootstrap
  :init-port-max)

(defun %access-port (ports enum)
  (mem-aref ports 'port
            (foreign-enum-value 'init-port enum)))

(defcfun ("exec_reauth_finished" exec-reauth-finished)
  :boolean
  (pid pid-t))

(defcfun ("helper_exec_reauth" %helper-exec-reauth)
  pid-t
  (newauth auth-t)
  (secure :boolean)
  (ports :pointer)
  (num-ports :unsigned-int)
  (fds :pointer)
  (num-fds :unsigned-int)
  (is-empty :boolean)
  (uid uid-t))

(defun exec-reauth (auth
                    use-suid-p
                    uid
                    use-sgid-p
                    gid
                    fallback-user
                    ports
                    num-ports
                    fds
                    num-fds
                    wait-fun)
  (let ((is-root-p nil)
        (secure nil)
        (newauth nil))
    ; Fetch user current id's.
    (multiple-value-bind (eff-user avail-user)
      (auth-getids (%access-port ports :init-port-auth))
      (setf is-root-p (or (contains-uid-p eff-user 0)
                          (contains-uid-p avail-user 0)))
      (when use-suid-p
        (setf secure (or (uid-setid eff-user avail-user uid)
                         secure))
      (when use-sgid-p
        (setf secure (or (gid-setid eff-user avail-user gid)
                         secure)))
      (multiple-value-bind (ret err)
        (auth-makeauth auth
                       (list (%access-port ports :init-port-auth))
                       :copy-send
                       eff-user
                       avail-user)
        (when ret
          (setf newauth ret))
        (when (and err
                   (not (eq err :invalid-argument)))
          (return-from exec-reauth err))
        (when (and (eq err :invalid-argument)
                   fallback-user)
          (clear-user avail-user)
          (setf eff-user fallback-user)
          (setf is-root-p (contains-uid-p eff-user 0))
          (when use-suid-p
            (setf secure (or (uid-setid eff-user avail-user uid)
                             secure)))
          (when use-sgid-p
            (setf secure (or (gid-setid eff-user avail-user gid)
                             secure)))
          (multiple-value-bind (ret err)
            (auth-makeauth auth
                           (list)
                           :copy-send
                           eff-user
                           avail-user)
            (when err
              (return-from exec-reauth err))
            (when ret
              (setf newauth ret))))
        (when is-root-p
          (setf secure nil))
        (let ((empty-p (empty-uids-p eff-user)))
          (let ((pid (%helper-exec-reauth newauth secure
                                          ports num-ports
                                          fds num-fds
                                          empty-p (if empty-p
                                                    0
                                                    (first (uids eff-user))))))
            (let (ret)
              (loop until ret
                    do (progn
                         (funcall wait-fun)
                         (setf ret (exec-reauth-finished pid)))))))
        secure)))))
