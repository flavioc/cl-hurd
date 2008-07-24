
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
             (let ((found-old-node (dir-lookup *translator*
                                               (get-node old-dir-protid)
                                               (get-user old-dir-protid)
                                               old-name)))
               (unless found-old-node
                 (return-from dir-rename :no-such-file))
               (let ((found-new-node (dir-lookup *translator*
                                                 (get-node new-dir-protid)
                                                 (get-user new-dir-protid)
                                                 new-name)))
                 (when (and found-new-node excl)
                   (return-from dir-rename :file-exists))
                 (let ((return-code (file-rename *translator*
                                                 (get-user old-dir-protid)
                                                 (get-node old-dir-protid)
                                                 old-name
                                                 (get-node new-dir-protid)
                                                 new-name)))
                   (when (eq t return-code)
                     (deallocate-send-right new-dir-protid))
                   (cond
                     ((eq t return-code) t)
                     ((eq nil return-code) :permission-denied)
                     (t return-code)))))))))


