
(in-package :hurd-translator)

(def-fs-interface :file-getlinknode ((port port)
									 (ret-port port-pointer)
									 (ret-type :pointer))
  (with-lookup protid port
    (block file-getlinknode
           (unless (eq (root *translator*) (get-node protid))
             (return-from file-getlinknode :resource-busy))
           (setf (mem-ref ret-port 'port) (get-right protid))
           (setf (mem-ref ret-type 'msg-type-name) :make-send)
           t)))

