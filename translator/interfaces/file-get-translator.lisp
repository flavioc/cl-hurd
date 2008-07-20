
(in-package :hurd-translator)

(defun %get-translator-list (node user)
  (let ((what (get-type (stat node))))
    (warn "%get-translator-list ~s" what)
    (case what
      (:lnk
        (when (and (link node)
                   (allow-link-p *translator* node user))
          (list +hurd-symlink+ (link node))))
      ((:chr :blk)
       (let ((device (stat-get (stat node) 'rdev)))
         (list (if (eq what :chr) +hurd-chrdev+ +hurd-blkdev+)
               (prin1-to-string (major device))
               (prin1-to-string (minor device)))))
      (:fifo (list +hurd-fifo+))
      (:sock (list +hurd-ifsock+))
      (t nil))))

(def-fs-interface :file-get-translator ((file port)
                                        (data :pointer)
                                        (data-len :pointer))
  (with-lookup protid file
    (let ((node (get-node protid))
          (user (get-user protid)))
      (let ((arg-list (translator node)))
        (unless arg-list
          ; No passive translator set. Try to get a short circuited one
          (setf arg-list (%get-translator-list node user)))
        (when arg-list
          (let* ((len-args (string-list-len arg-list))
                 (total (sum-list len-args)))
            (when (> total (mem-ref data-len 'msg-type-number))
              (setf (mem-ref data :pointer)
                    (mmap (make-pointer 0)
                          total
                          '(:prot-read :prot-write)
                          '(:map-anon)
                          0
                          0)))
            (setf (mem-ref data-len 'msg-type-number) total)
            (list-to-foreign-string-zero-separated arg-list
                                                   (mem-ref data :pointer)
                                                   len-args)
            (warn "arg-list ~s" arg-list)
            t))))))