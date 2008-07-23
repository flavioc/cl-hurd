
(in-package :hurd-translator)

(def-fs-interface :dir-mkdir ((dir port)
                              (name :string)
                              (mode mode-t))
  (with-lookup protid dir
    (let ((node (get-node protid))
          (user (get-user protid)))
      (cond
        ((not (is-dir-p (stat node)))
         :not-directory)
        (t
          (set-spare mode nil)
          (set-vtx mode nil)
          (set-type mode :dir)
          (let ((result (create-directory *translator*
                                          node
                                          user
                                          name
                                          mode)))
            (if result
              t
              :not-permitted)))))))

