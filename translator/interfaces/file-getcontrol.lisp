
(in-package :hurd-translator)

;; Return control port for this filesystem.
(def-fs-interface :file-getcontrol ((file port)
                                    (control port-pointer)
                                    (control-type :pointer))
  (with-lookup protid file
    (block file-getcontrol
           (unless (is-controller-p (get-node protid)
                                    (get-user protid))
             (return-from file-getcontrol :not-permitted))
           (let ((port (bucket-add-control-port (port-bucket *translator*))))
             (setf (mem-ref control 'port) (get-right port))
             (setf (mem-ref control-type 'msg-type-name) :make-send)
             t))))

