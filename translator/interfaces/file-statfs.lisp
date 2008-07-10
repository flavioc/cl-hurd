
(in-package :hurd-translator)

;; Return filesystem status.
(def-fs-interface :file-statfs ((file port)
                                (statfs statfs-t))
  (with-lookup protid file
    (when (refresh-statfs *translator* (get-user protid))
      (statfs-copy statfs (get-statfs *translator*))
      t)))

