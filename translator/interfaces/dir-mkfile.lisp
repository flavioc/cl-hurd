
(in-package :hurd-translator)

(def-fs-interface :dir-mkfile ((dir port)
                               (flags open-flags)
                               (mode mode-t)
                               (new-file port-pointer)
                               (new-file-type :pointer))
  (with-lookup dir-protid dir
    (block dir-mkfile
           (let ((new-node (create-anonymous-file *translator*
                                                  (get-node dir-protid)
                                                  (get-user dir-protid)
                                                  mode)))
             (unless new-node
               (return-from dir-mkfile nil))
             (let* ((new-user (make-iouser :old (get-user dir-protid)))
                    (old-open-node (open-node dir-protid))
                    (new-protid (new-protid
                                  *translator*
                                  new-user
                                  (make-open-node new-node
                                                  (only-flags flags +open-flags+)
                                                  :copy old-open-node))))
               (setf (mem-ref new-file 'port) (get-right new-protid)
                     (mem-ref new-file-type 'msg-type-name) :make-send)
               t)))))

