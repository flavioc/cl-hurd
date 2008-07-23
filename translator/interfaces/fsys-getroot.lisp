
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
     (let* ((new-open-node (make-open-node
                             node
                             (disable-flags flags +open-flags+)
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

(defun %unsupported-root-file-p (stat flags)
  (and (or (is-sock-p stat)
           (is-blk-p stat)
           (is-chr-p stat)
           (is-fifo-p stat))
       (or
         (flag-is-p flags :read)
         (flag-is-p flags :write)
         (flag-is-p flags :exec))))

(defun %must-follow-translator-p (node flags)
  (and (translator node)
       (not (flag-is-p flags :notrans))
       (or (has-passive-trans-p (stat node))
           (box-translated-p (box node)))))

(defun %fsys-getroot (node flags dotdot user)
  (when (%must-follow-translator-p node flags)
    (warn "has translator in root!")
    (let* ((*current-node* node)
           (*current-dotdot* dotdot))
      (multiple-value-bind (retry retry-name port)
        (fetch-root (box node) 
                    dotdot flags user
                    #'get-translator-callback
                    (callback fetch-root-callback))
        ;(warn "fetch-root returned ~s ~s ~s" retry retry-name port)
        (unless (eq retry :no-such-file)
          (return-from %fsys-getroot (values retry port :move-send retry-name))))))
  (when (%unsupported-root-file-p (stat node) flags)
    (return-from %fsys-getroot nil))
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
               (%fsys-getroot node flags dotdot user)
               ;(warn "got from %fsys-getroot ~s ~s ~s ~s"
               ;      retry-type0 file0 file-poly0 retry-name0)
               (setf (mem-ref retry-type 'retry-type) retry-type0
                     (mem-ref file 'port) file0
                     (mem-ref file-poly 'msg-type-name) file-poly0)
               (lisp-string-to-foreign retry-name0
                                       retry-name
                                       (1+ (length retry-name0)))
               t)))))
