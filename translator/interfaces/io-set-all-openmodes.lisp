
(in-package :hurd-translator)

(def-io-interface :io-set-all-openmodes ((port port)
                                         (bits open-flags))
  (with-lookup protid port
    (setf (flags (open-node protid)) bits)
    t))

