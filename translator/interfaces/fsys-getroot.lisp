
(in-package :hurd-translator)

(defun get-translator-callback (node port)
  (unless (has-passive-trans-p (stat node))
    (return-from get-translator-callback :no-such-file))
  (let ((translator-path (get-translator *translator* node)))
    (when translator-path
      (values translator-path
              (stat-get (stat node) 'uid)
              (stat-get (stat node) 'gid)))))

(defcallback fetch-root-callback err
             ((node-port :pointer)
              (root-parent :pointer)
              (flags open-flags-t)
              (underlying port-pointer)
              (underlying-type :pointer))
  (let* ((node (get-temporary-data *translator* node-port))
         (stat (stat node))
         (iouser (make-iouser :uids (stat-get stat 'uid)
                              :gids (stat-get stat 'gid)))
         (root-parent-port (mem-ref root-parent :int))
         (new (new-protid *translator* iouser
                          (make-open-node node flags
                                          :root-parent root-parent-port))))
    (setf (mem-ref underlying 'port) (get-right new))
    (setf (mem-ref underlying-type 'msg-type-name) :make-send)
    t))

(defun unsupported-root-file-p (stat flags)
  (and (or (is-sock-p stat)
           (is-blk-p stat)
           (is-chr-p stat)
           (is-fifo-p stat))
       (or
         (flag-is-p flags :read)
         (flag-is-p flags :write)
         (flag-is-p flags :exec))))

(def-fsys-interface :fsys-getroot ((fsys port)
								   (reply port)
								   (reply-poly msg-type-name)
								   (dotdot port)
								   (gen-uids :pointer)
								   (gen-uids-count msg-type-number)
								   (gen-gids :pointer)
								   (gen-gids-count msg-type-number)
								   (flags open-flags-t)
								   (retry-type :pointer)
								   (retry-name :pointer)
								   (file port-pointer)
								   (file-poly :pointer))
  (with-accessors ((root-node root)) *translator*
    (when (and root-node
               (port-exists-p fsys))
      (let ((user (make-iouser-mem gen-uids gen-uids-count
                                   gen-gids gen-gids-count)))
        (only flags :hurd)
        (block outer-block
               (when (and (or (has-passive-trans-p (stat root-node))
                              (box-translated-p (box root-node)))
                          (flag-is-p flags :notrans))
                 (insert-temporary-data *translator* fsys root-node)
                 (let ((ret-fetch (fetch-root (box root-node) fsys
                                              dotdot flags
                                              gen-uids gen-uids-count
                                              gen-gids gen-gids-count
                                              #'get-translator-callback (callback fetch-root-callback)
                                              retry-type retry-name file)))
                   (remove-temporary-data *translator* fsys)
                   (unless (eq ret-fetch :no-such-file)
                     (if (eq ret-fetch t)
                       (setf (mem-ref file-poly 'msg-type-name) :move-send))
                     (return-from outer-block ret-fetch))))
               (when (unsupported-root-file-p (stat root-node) flags)
                 (warn "unsupported!")
                 (return-from outer-block nil))
               (unless (allow-open *translator* root-node user flags t)
                 (return-from outer-block :not-permitted))
               (disable flags :open-modes)
               (let ((new (new-protid *translator* user
                                      (make-open-node (root *translator*)
                                                      flags
                                                      :root-parent dotdot))))
                 (port-deallocate dotdot)
                 (setf (mem-ref retry-type 'retry-type) :retry-normal)
                 (setf (mem-ref file 'port) (get-right new))
                 (setf (mem-ref file-poly 'msg-type-name) :make-send)
                 (lisp-string-to-foreign "" retry-name 1)
                 t))))))

