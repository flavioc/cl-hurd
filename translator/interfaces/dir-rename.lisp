
(in-package :hurd-translator)

(def-fs-interface :dir-rename ((old-dir port)
                               (old-name :string)
                               (new-dir port)
                               (new-name :string)
                               (excl :boolean))
  (with-lookup old-dir-protid old-dir
    (block dir-rename
           (unless (port-exists-p new-dir)
             (return-from dir-rename :invalid-cross-device-link))
           (with-lookup new-dir-protid new-dir
             (let* ((old-dir-node (get-node old-dir-protid))
                    (old-dir-user (get-user old-dir-protid))
                    (found-old-node (directory-lookup *translator*
                                                      old-dir-node
                                                      old-dir-user
                                                      old-name)))
               (unless found-old-node
                 (return-from dir-rename :no-such-file))
               (let* ((new-dir-node (get-node new-dir-protid))
                      (new-dir-user (get-user new-dir-protid))
                      (found-new-node (directory-lookup *translator*
                                                        new-dir-node
                                                        new-dir-user
                                                        new-name)))
                 (when (and found-new-node excl)
                   (return-from dir-rename :file-exists))
                 (let ((return-code (file-rename *translator*
                                                 old-dir-user
                                                 old-dir-node
                                                 old-name
                                                 new-dir-node
                                                 new-name)))
                   (when (eq t return-code)
                     (deallocate-send-right new-dir-protid))
                   (cond
                     ((eq t return-code) t)
                     ((eq nil return-code) :permission-denied)
                     (t return-code)))))))))

