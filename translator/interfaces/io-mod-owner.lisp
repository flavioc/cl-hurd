
(in-package :hurd-translator)

(def-io-interface :io-mod-owner ((port port)
								 (new-owner pid-t))
  (with-lookup protid port
    (setf (owner (get-node protid)) new-owner)
    t))
