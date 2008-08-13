
(in-package :hurd-translator)

(def-fs-interface :file-set-size ((file port)
                                  (size off-t))
  (with-lookup protid file
    (let* ((node (get-node protid))
           (err (file-change-size *translator*
                                  node
                                  (get-user protid)
                                  size)))
      (cond
        ((eq err t)
         (let ((open (open-node protid)))
           (when (> (file-offset open) size)
             (setf (file-offset open) size)))
         t)
        ((eq err nil) :not-permitted)
        (t err)))))

