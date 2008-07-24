
(in-package :hurd-translator)

;; Change file size.
(def-fs-interface :file-set-size ((file port)
                                  (size loff-t))
  (with-lookup protid file
    (let ((err (file-change-size *translator*
                                 (get-node protid)
                                 (get-user protid)
                                 size)))
      (cond
        ((eq err nil) :not-permitted)
        (t t)))))
