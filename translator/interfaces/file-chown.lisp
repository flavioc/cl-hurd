
(in-package :hurd-translator)

(def-fs-interface :file-chown ((file port)
							   (owner uid-t)
							   (group gid-t))
  (with-lookup protid file
    (let ((err (file-chown *translator*
                           (get-node protid)
                           (get-user protid)
                           owner
                           group)))
      (cond
        ((eq err nil) :not-permitted)
        ((eq err t) t)
        (t err)))))
