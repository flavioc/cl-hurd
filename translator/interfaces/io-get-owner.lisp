
(in-package :hurd-translator)

(def-io-interface :io-get-owner ((port port)
								 (owner :pointer))
  (with-lookup protid port
    (setf (mem-ref owner 'pid-t) (owner (get-node protid)))
    t))

