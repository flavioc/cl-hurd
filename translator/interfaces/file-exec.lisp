
(in-package :hurd-translator)

(defun %deallocate-port-array (ptr len)
  (loop for i from 0 below len
        do (port-deallocate
             (mem-aref ptr 'port i))))

(defun print-str-ptr (ptr len)
  (loop for i from 0 below len
        do (format *error-output*
                   "~c" (code-char (mem-aref ptr :char i))))
  (format *error-output* "~%"))

(def-fs-interface :file-exec ((file port)
                              (task task)
                              (flags exec-flags)
                              (argv :pointer)
                              (argvlen :unsigned-int)
                              (envp :pointer)
                              (envplen :unsigned-int)
                              (fds :pointer)
                              (fdslen :unsigned-int)
                              (portarray :pointer)
                              (portarraylen :unsigned-int)
                              (intarray :pointer)
                              (intarray-len :unsigned-int)
                              (deallocnames :pointer)
                              (deallocnameslen :unsigned-int)
                              (destroynames :pointer)
                              (destroynameslen :unsigned-int))
  (with-lookup protid file
    (warn "gonna exec..")
    (block file-exec
           (let ((node (get-node protid))
                 (open (open-node protid))
                 (user (get-user protid)))
             (unless (flag-is-p (flags open) :exec)
               (return-from file-exec :bad-fd))
             (unless (has-access-p node user 'exec)
               (return-from file-exec :permission-denied))
             (when (is-dir-p (stat node))
               (return-from file-exec :permission-denied))
             (when (or (is-uid-p (stat node))
                       (is-gid-p (stat node)))
               (warn "hmm fail")
               ;; Do reauth stuff
             )
             (warn "creating stuff..")
             (let* ((new-user (make-iouser :old user))
                    (new-open (make-open-node node
                                              '(:read)
                                              :root-parent (root-parent open)
                                              :shadow-root (shadow-root open)
                                              :shadow-root-parent (shadow-root-parent open)))
                    (new-protid (new-protid *translator*
                                            new-user
                                            new-open)))
               (with-port-deallocate (right (get-send-right new-protid))
                 (warn "going to exec-exec. ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s"
                       +exec-server+ right :copy-send task flags
                       argv argvlen
                       envp envplen
                       fds :copy-send fdslen
                       portarray :copy-send portarraylen
                       intarray intarray-len
                       deallocnames deallocnameslen
                       destroynames destroynameslen)
                 (print-str-ptr argv argvlen)
                 (setf flags (enable-flags flags :newtask))
                 (let ((ret (exec-exec +exec-server+
                                       right
                                       :copy-send
                                       task
                                       flags
                                       argv argvlen
                                       envp envplen
                                       fds :copy-send fdslen
                                       portarray :copy-send portarraylen
                                       intarray intarray-len
                                       deallocnames deallocnameslen
                                       destroynames destroynameslen)))
                   (warn "exec-exec returned with ~s" ret)
                   (cond
                     ((eq t ret)
                      (port-deallocate task)
                      (%deallocate-port-array fds fdslen)
                      (%deallocate-port-array portarray portarraylen)
                      t)
                     (t
                       ret)))))))))
