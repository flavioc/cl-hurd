
(in-package :hurd-translator)

(def-io-interface :io-clear-some-openmodes ((port port)
											(bits open-flags-t))
  (with-lookup protid port
    (disable (flags (open-node protid))
             bits)
    t))

