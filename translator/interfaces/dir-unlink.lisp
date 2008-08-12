
(in-package :hurd-translator)

(def-fs-interface :dir-unlink ((port port)
                               (name :string))
  (with-lookup protid port
    (let* ((node (get-node protid))
           (user (get-user protid))
           (found-node (directory-lookup *translator*
                                         node
                                         user
                                         name)))
      (cond
        (found-node
          (let ((err (remove-directory-entry *translator*
                                             node
                                             user
                                             name)))
            (cond
              ((eq err t) t)
              ((eq err nil) :not-permitted)
              (t err))))
        (t :no-such-file)))))

