
(in-package :hurd-translator)

(def-io-interface :io-get-openmodes ((port port)
									 (bits :pointer))
  (with-lookup protid port
    (setf (mem-ref bits 'open-flags)
          (only-flags (flags (open-node protid))
                      +honored-get-modes+))
    t))

