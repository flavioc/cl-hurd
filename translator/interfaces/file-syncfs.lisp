
(in-package :hurd-translator)

(def-fs-interface :file-syncfs ((port port)
                                (wait :boolean)
                                (do-children :boolean))
  (declare (ignore do-children))
  (with-lookup protid port
    (if (sync-fs *translator*
                     (get-user protid)
                     wait)
      t
      nil)))

