
(in-package :hurd-translator)

(def-fs-interface :file-reparent ((file port)
                                  (parent port)
                                  (new-file port-pointer)
                                  (new-file-type :pointer))
  (with-lookup protid file
    (let* ((node (get-node protid))
           (user (get-user protid))
           (open-node (open-node protid))
           (copy-user (make-iouser :old user))
           (new-open-node (make-open-node
                            node
                            (flags open-node)
                            :copy open-node))
           (new (new-protid *translator*
                            copy-user
                            new-open-node)))
      (install-shadow-root new-open-node node parent)
      (setf (mem-ref new-file 'port) (get-right new)
            (mem-ref new-file-type 'msg-type-name) :make-send)
      t)))

