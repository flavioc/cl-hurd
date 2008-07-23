
(in-package :hurd-translator)

(def-io-interface :io-clear-some-openmodes ((port port)
											(old-flags open-flags))
  (with-lookup protid port
    (setf (flags (open-node protid))
          (disable-flags (flags (open-node protid))
                         (only-flags old-flags
                                     +honored-open-modes+)))
    t))

