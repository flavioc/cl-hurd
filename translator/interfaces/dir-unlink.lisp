
(in-package :hurd-translator)

(def-fs-interface :dir-unlink ((port port)
							   (name :string))
  (with-lookup protid port
    (if (remove-directory-entry *translator*
                                (get-node protid)
                                (get-user protid)
                                name
                                nil) ; Not a directory
      t
      :not-permitted)))
