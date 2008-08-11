
(in-package :hurd)

(defcfun ("helper_fetch_root" %helper-fetch-root)
  err
  (dotdot port)
  (fetch-root-callback :pointer)
  (uid uid-t)
  (gid gid-t)
  (argz :pointer)
  (argz-len :unsigned-int)
  (control-port port-pointer))

(defun %fetch-root (dotdot fetch-root-callback uid gid translator)
  (let* ((len-args (string-list-len translator))
         (argz-len (sum-list len-args)))
    (with-foreign-pointer (control (foreign-type-size 'port))
      (with-foreign-pointer (argz argz-len)
        (list-to-foreign-string-zero-separated translator argz len-args)
        (let ((return-code (%helper-fetch-root dotdot
                                               fetch-root-callback
                                               uid
                                               gid
                                               argz
                                               argz-len
                                               control)))
          (select-error return-code (mem-ref control 'port)))))))

(defun %try-start-translator (dotdot fetch-root-callback path uid gid)
  (multiple-value-bind (ret err)
    (%fetch-root dotdot fetch-root-callback uid gid path)
    (cond
      (err err)
      ((port-valid-p ret) ret)
      (t :translator-died))))

(defun fetch-root (box dotdot flags user get-translator-callback fetch-root-callback)
  "Fetch the child translator port, starting the passive translator if needed."
  (unless (box-active-p box)
    (multiple-value-bind (path uid gid) (funcall get-translator-callback box)
      (unless (and path uid gid)
        (return-from fetch-root path)) ; return error
      (let ((control (%try-start-translator dotdot
                                            fetch-root-callback
                                            path
                                            uid
                                            gid)))
        (unless (port-valid-p control)
          (return-from fetch-root control)) ; error
        ; Set the now _active_ translator port
        (box-set-active box control t))))
  ; If we have come this far, it means that the box has an active port now!
  (let ((control (active box)))
    (port-mod-refs control :right-send 1)
    (multiple-value-bind (retry retry-name port)
        (fsys-getroot control dotdot :copy-send user flags)
      (port-deallocate control)
      (cond
        ((not (port-valid-p port))
         (box-set-active box nil nil)
         nil)
        (t
          (values retry retry-name port))))))

