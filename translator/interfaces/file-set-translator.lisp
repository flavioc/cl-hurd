
(in-package :hurd-translator)

(defun %get-short-circuited-translator (ls)
  (let ((name (first ls)))
    (cond
      ((string= name +hurd-symlink+) :lnk)
      ((string= name +hurd-chrdev+) :chr)
      ((string= name +hurd-blkdev+) :blk)
      ((string= name +hurd-fifo+) :fifo)
      ((string= name +hurd-ifsock+) :sock)
      (t nil))))

(defun %set-short-circuited-translator (ls node user)
  (let ((what (%get-short-circuited-translator ls)))
    (warn "ls ~s" ls)
    (when what
      (case what
        (:lnk
          (when (= (length ls) 2)
            (create-symlink *translator* node user (second ls))))
        ((:blk :chr)
         (when (= (length ls) 3)
           (let ((major (parse-integer (second ls) :junk-allowed t))
                 (minor (parse-integer (third ls) :junk-allowed t)))
             (warn "major ~s minor ~s" major minor)
             (when (and major minor)
               (warn "major minor ok")
               (let ((device (make-instance 'device-id
                                            :major major
                                            :minor minor)))
                 (warn "device created..")
                 (if (eq what :blk)
                   (create-block *translator* node user device)
                   (create-character *translator* node user device)))))))
        (:fifo
          (when (= (length ls) 1)
            (create-fifo *translator* node user)))
        (:sock
          (when (= (length ls) 1)
            (create-socket *translator* node user)))
        (otherwise nil)))))

(defun %try-short-circuited-translator (passive-flags
                                         passive-list
                                         node user)
  (unless (flag-is-p passive-flags :force)
    (%set-short-circuited-translator passive-list node user)))

(def-fs-interface :file-set-translator ((file port)
                                        (passive-flags fs-trans-flags)
                                        (active-flags fs-trans-flags)
                                        (killtrans-flags fsys-goaway-flags)
                                        (passive :pointer)
                                        (passivelen msg-type-number)
                                        (active port))
  (with-lookup protid file
    (block set-translator
           (warn "file-set-translator active ~s passive ~s activeport ~s killtrans ~s"
                 active-flags passive-flags active killtrans-flags)
           (unless (or (flag-is-p passive-flags :set)
                       (flag-is-p active-flags :set))
             (return-from set-translator :invalid-argument))
           (let ((node (get-node protid))
                 (user (get-user protid)))
             (when (and (flag-is-p active-flags :set)
                        (not (flag-is-p active-flags :orphan)))
               (unless (is-owner-p node user)
                 (return-from set-translator :permission-denied))
               (when (and (box-active-p (box node))
                          (not (flag-is-p active-flags :excl)))
                 (let* ((control (box-fetch-control (box node)))
                        (away-err (fsys-goaway control
                                               killtrans-flags)))
                   (unless (or (eq t away-err)
                               (eq :server-died away-err)
                               (eq :send-invalid-dest away-err))
                     (return-from set-translator away-err)))))
             (when (and (flag-is-p passive-flags '(:set :excl))
                        (box-passive-p (box node)))
               (return-from set-translator :resource-busy))
             (when (flag-is-p active-flags :set)
               (unless (box-set-active (box node) active
                                       (flag-is-p active-flags :excl))
                 (return-from set-translator :resource-busy)))
             (cond
               ((flag-is-p passive-flags :set)
                (let* ((passive-list (foreign-string-zero-separated-to-list
                                       passive passivelen))
                       (sct (%try-short-circuited-translator
                              passive-flags
                              passive-list
                              node
                              user)))
                  (cond
                    (sct t)
                    (t
                      (warn "going to set...")
                      (box-set-passive (box node)
                                       passive-list)))))
               ((flag-is-p active-flags :set)
                t))))))
