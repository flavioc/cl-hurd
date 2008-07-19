
(in-package :hurd-translator)

(defun %get-short-circuited-translator (ls)
  (let ((name (first ls)))
    (cond
      ((string= name +hurd-symlink+)
       :lnk)
      ((string= name +hurd-chrdev+)
       :chr)
      ((string= name +hurd-blkdev+)
       :blk)
      ((string= name +hurd-fifo+)
       :fifo)
      ((string= name +hurd-ifsock+)
       :sock)
      (t
        nil))))

(defun %set-short-circuited-translator (ls node user)
  (let ((what (%get-short-circuited-translator ls)))
    (when what
      (case what
        (:lnk
          (when (= (length ls) 2)
            (create-symlink *translator* node user (second ls))))
        (otherwise
          nil)))))

(def-fs-interface :file-set-translator ((file port)
                                        (passive-flags fs-trans-flags)
                                        (active-flags fs-trans-flags)
                                        (killtrans-flags fsys-goaway-flags)
                                        (passive :pointer)
                                        (passivelen msg-type-number)
                                        (active port))
  (with-lookup protid file
    (block set-translator
           (unless (or (flag-is-p passive-flags :set)
                       (flag-is-p active-flags :set))
             (return-from set-translator :invalid-argument))
           (let ((node (get-node protid))
                 (user (get-user protid)))
             (when (and (flag-is-p active-flags :set)
                        (not (flag-is-p active-flags :orphan)))
               (unless (is-owner-p node user)
                 (return-from set-translator :permission-denied))
               (when (and (box-translated-p (box node))
                          (not (flag-is-p active-flags :excl)))
                 (let ((away-err (fsys-goaway (box-fetch-control (box node))
                                              killtrans-flags)))
                   (unless (or (eq t away-err)
                               (eq :server-died away-err)
                               (eq :send-invalid-dest away-err))
                     (return-from set-translator away-err)))))
             (when (and (flag-is-p passive-flags '(:set :excl))
                        (has-passive-trans-p (stat node)))
               (return-from set-translator :resource-busy))
             (when (flag-is-p active-flags :set)
               (unless (box-set-active (box node) active
                                       (flag-is-p active-flags :excl))
                 (return-from set-translator :resource-busy)))
             (when (flag-is-p passive-flags :set)
               (let ((passive-list (foreign-string-zero-separated-to-list
                                     passive passivelen)))
                 (unless (flag-is-p passive-flags :force)
                   (let ((err-set (%set-short-circuited-translator
                                    passive-list node user)))
                     (cond
                       ((null err-set)
                        )
                       (t
                         err-set))))))))))
