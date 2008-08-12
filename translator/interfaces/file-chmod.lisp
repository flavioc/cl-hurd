
(in-package :hurd-translator)

(def-fs-interface :file-chmod ((file port)
                               (mode mode-t))
  (with-lookup protid file
    ; We only want permission bits
    ; Disable those bits that don't make much sense here
    (set-spare mode nil)
    (set-types mode nil)
    (set-trans mode nil)
    (let ((err (chmod-file *translator*
                           (get-node protid)
                           (get-user protid)
                           mode)))
      (cond
        ((eq err t) t)
        ((eq err nil) :not-permitted)
        (t err)))))

