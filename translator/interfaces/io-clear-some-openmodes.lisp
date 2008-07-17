
(in-package :hurd-translator)

(def-io-interface :io-clear-some-openmodes ((port port)
											(old-flags open-flags))
  (with-lookup protid port
    (setf (flags (open-node protid))
          (disable-flags (flags (open-node protid))
                         old-flags))
    t))

