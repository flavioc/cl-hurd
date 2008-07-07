
(in-package :hurd-translator)

(def-fs-interface :file-chown ((file port)
							   (owner uid-t)
							   (group gid-t))
  (with-lookup protid file
    (file-chown *translator*
                (get-node protid)
                (get-user protid)
                owner
                group)))
