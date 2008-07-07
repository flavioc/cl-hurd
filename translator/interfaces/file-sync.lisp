
(in-package :hurd-translator)

(def-fs-interface :file-sync ((port port)
							  (wait :boolean)
							  (omit-meta :boolean))
  (with-lookup protid port
    (if (file-sync *translator*
                   (get-node protid)
                   (get-user protid)
                   wait
                   omit-meta)
      t
      nil)))
