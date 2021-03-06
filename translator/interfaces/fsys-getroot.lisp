
(in-package :hurd-translator)

(defun %must-follow-symlink-p (node user flags)
  (and (is-lnk-p (stat node))
       (link node)
       (not (flag-is-p flags :nolink))
       (not (flag-is-p flags :notrans))
       (allow-link-p *translator* node user)))

(defun %handle-normal-file (node flags dotdot user)
  (cond
    ((allow-open-p *translator* node user flags t)
     (when (flag-is-p flags :trunc)
       (unless (file-change-size *translator* node user 0)
         (return-from %handle-normal-file :not-permitted)))
     (let* ((new-open-node (make-open-node
                             node
                             (disable-flags flags +open-create-flags+)
                             :root-parent dotdot))
            (new (new-protid *translator*
                            user
                            new-open-node)))
       (port-deallocate dotdot)
       (values :retry-normal
               (get-right new)
               :make-send
               "")))
    (t :not-permitted)))

(defun %handle-symlink (node dotdot)
  (let ((target (link node)))
    (cond
      ((eq (char target 0) #\/) ; Points to root.
       (port-deallocate dotdot)
       (values :retry-magical
               nil
               :copy-send
               target))
      (t
        (values :retry-reauth
                dotdot
                :move-send
                target)))))

(defun %fsys-getroot-normal (node flags dotdot user)
  (cond
    ((%must-follow-symlink-p node user flags)
     (%handle-symlink node dotdot))
    (t (%handle-normal-file node flags dotdot user))))

(defun %must-follow-translator-p (node flags)
  (and (not (flag-is-p flags :notrans))
       (box-translated-p (box node))))

(%add-callback do-fsys-getroot (node flags dotdot user)
  "Lookup root port in 'node' with 'flags' to 'user'. 'dotdot' is the parent directory.
This must return four things:
Type of retry.
Retry port.
Retry port type.
Filename to retry.
"
  (when (%must-follow-translator-p node flags)
    (let* ((*current-node* node)
           (*current-dotdot* dotdot))
      (multiple-value-bind (retry retry-name port)
        (fetch-root (box node) 
                    dotdot flags user
                    #'get-translator-callback
                    (callback fetch-root-callback))
        (unless (eq retry :no-such-file)
          (return-from do-fsys-getroot (values retry port :move-send retry-name))))))
  (%fsys-getroot-normal node flags dotdot user))

(def-fsys-interface :fsys-getroot ((fsys port)
                                   (reply port)
                                   (reply-poly msg-type-name)
                                   (dotdot port)
                                   (gen-uids :pointer)
                                   (gen-uids-count msg-type-number)
                                   (gen-gids :pointer)
                                   (gen-gids-count msg-type-number)
                                   (flags open-flags)
                                   (retry-type :pointer)
                                   (retry-name :pointer)
                                   (file port-pointer)
                                   (file-poly :pointer))
  (declare (ignore reply reply-poly))
  (with-accessors ((node root)) *translator*
    (block getroot
           (unless (and node (port-exists-p fsys))
             (return-from getroot nil))
           (let ((user (make-iouser-mem gen-uids gen-uids-count
                                        gen-gids gen-gids-count)))
             (multiple-value-bind (retry-type0 file0 file-poly0 retry-name0)
               (do-fsys-getroot *translator* node flags dotdot user)
               (cond
                 ((null retry-name0) retry-type0) ; Some error ocurred
                 (t
                   (setf (mem-ref retry-type 'retry-type) retry-type0
                         (mem-ref file 'port) file0
                         (mem-ref file-poly 'msg-type-name) file-poly0)
                   (lisp-string-to-foreign retry-name0
                                           retry-name
                                           (1+ (length retry-name0)))
                   t)))))))

