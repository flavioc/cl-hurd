
(in-package :hurd-translator)

(def-io-interface :io-set-some-openmodes ((port port)
                                          (bits open-flags-t))
  (with-lookup protid port
    (enable (flags (open-node protid)) bits)
    t))

