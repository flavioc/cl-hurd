
(in-package :hurd-translator)

(defcfun ("do_exec_exec" %do-exec-exec)
  pid-t
  (execserver port)
  (file port)
  (file-type msg-type-name)
  (oldtask task)
  (flags exec-flags)
  (argv :pointer)
  (argvlen msg-type-number)
  (envp :pointer)
  (envplen msg-type-number)
  (dtable :pointer)
  (dtable-type msg-type-name)
  (dtablelen msg-type-number)
  (portarray :pointer)
  (portarray-type msg-type-name)
  (portarraylen msg-type-number)
  (intarray :pointer)
  (intarraylen msg-type-number)
  (deallocnames :pointer)
  (deallocnameslen msg-type-number)
  (destroynames :pointer)
  (destroynameslen msg-type-number))

(defcfun ("exec_finished" %exec-finished)
  :boolean
  (pid pid-t)
  (status :pointer))

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
    (block file-exec
           (let ((node (get-node protid))
                 (open (open-node protid))
                 (user (get-user protid)))
             (unless (flag-is-p (flags open) :exec)
               (return-from file-exec :bad-fd))
             (unless (has-access-p node user :exec)
               (return-from file-exec :permission-denied))
             (when (is-dir-p (stat node))
               (return-from file-exec :permission-denied))
             (let ((use-uid-p (is-uid-p (stat node)))
                   (use-gid-p (is-gid-p (stat node))))
               (when (or use-uid-p use-gid-p)
                 (warn "suid/sgid executables not supported.")))
             (let* ((new-user (make-iouser :old user))
                    (new-open (make-open-node node
                                              '(:read)
                                              :copy open))
                    (new-protid (new-protid *translator*
                                            new-user
                                            new-open)))
               (with-port-deallocate (right (get-send-right new-protid))
                 (let ((pid
                         (%do-exec-exec +exec-server+
                                        right
                                        :copy-send
                                        task
                                        (enable-flags flags :newtask)
                                        argv argvlen
                                        envp envplen
                                        fds :copy-send fdslen
                                        portarray :copy-send portarraylen
                                        intarray intarray-len
                                        deallocnames deallocnameslen
                                        destroynames destroynameslen)))
                   (when (zerop pid)
                     (return-from file-exec :gratuitous-error))
                   (with-foreign-pointer (status (foreign-type-size :int))
                     (loop for ret = (%exec-finished pid status)
                           when ret
                           return (mem-ref status 'err)
                           do (wait :miliseconds 200))))))))))
