
(in-package :hurd-translator)

(def-io-interface :io-set-all-openmodes ((port port)
                                         (bits open-flags))
  (with-lookup protid port
    (setf (flags (open-node protid))
          (enable-flags
            (disable-flags (flags (open-node protid))
                           +honored-open-modes+)
            (only-flags bits +honored-open-modes+)))
    t))

