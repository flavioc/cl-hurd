
(in-package :hurd-translator)

(define-helper-library file-exec)

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

(defun exec-finished (pid)
  (with-foreign-pointer (status (foreign-type-size :int))
    (let ((err (%exec-finished pid status)))
      (cond
        ((null err) nil)
        (t (mem-ref status 'err))))))

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
             (when (or (is-uid-p (stat node))
                       (is-gid-p (stat node)))
               (warn "hmm fail")
               ;; Do reauth stuff
             )
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
                   (let (ret)
                     (loop until ret
                           do (progn
                                (wait :miliseconds 200)
                                (setf ret (exec-finished pid))))
                     ret))))))))
