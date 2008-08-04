
(in-package :hurd-translator)

(def-fs-interface :dir-link ((dir port)
                             (file port)
                             (name :string)
                             (excl :boolean))
  (block dir-link
         (with-lookup dir-protid dir
           (unless (port-exists-p file)
             ; Only nodes in this translator!
             (return-from dir-link :invalid-cross-device-link))
           (with-lookup file-protid file
             (let ((dir-node (get-node dir-protid))
                   (dir-user (get-user dir-protid))
                   (found-node (directory-lookup *translator*
                                                 dir-node
                                                 dir-user
                                                 name)))
               (when (and found-node excl)
                 (return-from dir-link :file-exists))
               (let ((ret-code (create-hard-link *translator*
                                                 dir-node
                                                 dir-user
                                                 (get-node file-protid)
                                                 name)))
                 (cond
                   ((eq ret-code t)
                     (deallocate-send-right file-protid)
                     t)
                   ((eq ret-code nil) :not-permitted)
                   (t ret-code))))))))

