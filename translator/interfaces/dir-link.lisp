
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
             (let ((found-node (dir-lookup *translator*
                                           (get-node dir-protid)
                                           (get-user dir-protid)
                                           name)))
               (when (and found-node excl)
                 (return-from dir-link :file-exists))
               (let ((ret-code (create-hard-link *translator*
                                                 (get-node dir-protid)
                                                 (get-user dir-protid)
                                                 (get-node file-protid)
                                                 name)))
                 (cond
                   ((eq ret-code t)
                     (deallocate-send-right file-protid)
                     t)
                   ((eq ret-code nil) :not-permitted)
                   (t ret-code))))))))

