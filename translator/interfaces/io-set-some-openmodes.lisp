
(in-package :hurd-translator)

(def-io-interface :io-set-some-openmodes ((port port)
                                          (new-flags open-flags))
  (with-lookup protid port
    (setf (flags (open-node protid))
          (enable-flags (flags (open-node protid))
                        new-flags))
    t))

