
(in-package :hurd-translator)

(def-io-interface :io-get-openmodes ((port port)
									 (bits :pointer))
  (with-lookup protid port
    (setf (mem-ref bits 'open-flags) (flags (open-node protid)))
    t))

