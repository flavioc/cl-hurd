
(in-package :hurd-translator)

(defun print-str-ptr (ptr len)
  (loop for i from 0 below len
        do (format *error-output*
                   "~c" (code-char (mem-aref ptr :char i))))
  (format *error-output* "~%"))

(define-helper-library file-exec)

(defcfun ("do_exec_exec" %do-exec-exec)
  :pointer
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

(defun %exec-done-p (ptr)
  (mem-ref ptr :boolean))

(defun %exec-ret-code (ptr)
  (mem-ref (inc-pointer ptr (foreign-type-size :int)) 'err))

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
             (unless (has-access-p node user :exec)
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
                                              :copy open))
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
                 (warn "launching do-exec-exec")
                 (let ((ptr
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
                   (warn "do-exec-exec ~s" ptr)
                   (return-from file-exec nil)
                   (with-cleanup (foreign-free ptr)
                     (loop until (%exec-done-p ptr)
                           do (progn (warn "still not done")
                                     (wait :miliseconds 200)))
                     (%exec-ret-code ptr)))))))))
