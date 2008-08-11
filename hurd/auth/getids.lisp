
(in-package :hurd)

(defcfun ("auth_getids" %auth-getids)
  err
  (auth auth-t)
  (eff-uids :pointer)
  (num-eff-uids :pointer)
  (avail-uids :pointer)
  (num-avail-uids :pointer)
  (eff-gids :pointer)
  (num-eff-gids :pointer)
  (avail-gids :pointer)
  (num-avail-gids :pointer))

(defun auth-getids (handle)
  "Return the current user id's, effective and available."
  (let ((eff-uids (%new-ptr))
        (num-eff-uids (%new-unsigned 10))
        (avail-uids (%new-ptr))
        (num-avail-uids (%new-unsigned 20))
        (eff-gids (%new-ptr))
        (num-eff-gids (%new-unsigned 10))
        (avail-gids (%new-ptr))
        (num-avail-gids (%new-unsigned 20))
        (eff-uids-buf (foreign-alloc 'uid-t :count 10))
        (avail-uids-buf (foreign-alloc 'uid-t :count 20))
        (eff-gids-buf (foreign-alloc 'gid-t :count 10))
        (avail-gids-buf (foreign-alloc 'gid-t :count 20)))
    (with-cleanup (free-memory-list
                    (list eff-uids-buf avail-uids-buf eff-gids-buf avail-gids-buf
                          eff-uids avail-uids eff-gids avail-gids
                          num-eff-uids num-avail-uids num-eff-gids num-avail-gids))
      (setf (mem-ref eff-uids :pointer) eff-uids-buf
            (mem-ref avail-uids :pointer) avail-uids-buf
            (mem-ref eff-gids :pointer) eff-gids-buf
            (mem-ref avail-gids :pointer) avail-gids-buf)
      (let ((ret (%auth-getids handle
                               eff-uids
                               num-eff-uids
                               avail-uids
                               num-avail-uids
                               eff-gids
                               num-eff-gids
                               avail-gids
                               num-avail-gids)))
        (select-error ret
                      (let ((eff-uids-ptr (mem-ref eff-uids :pointer))
                            (num-eff-uids1 (mem-ref num-eff-uids :unsigned-int))
                            (avail-uids-ptr (mem-ref avail-uids :pointer))
                            (num-avail-uids1 (mem-ref num-avail-uids :unsigned-int))
                            (eff-gids-ptr (mem-ref eff-gids :pointer))
                            (num-eff-gids1 (mem-ref num-eff-gids :unsigned-int))
                            (avail-gids-ptr (mem-ref avail-gids :pointer))
                            (num-avail-gids1 (mem-ref num-avail-gids :unsigned-int)))
                        (with-cleanup (progn
                                        (unless (pointer-eq eff-uids-ptr
                                                            eff-uids-buf)
                                          (munmap eff-uids-ptr (* +uid-t-size+ num-eff-uids1)))
                                        (unless (pointer-eq avail-uids-ptr
                                                            avail-uids-buf)
                                          (munmap avail-uids-ptr (* +uid-t-size+ num-avail-uids1)))
                                        (unless (pointer-eq eff-gids-ptr
                                                            eff-gids-buf)
                                          (munmap eff-gids-ptr (* +gid-t-size+ num-eff-gids1)))
                                        (unless (pointer-eq avail-gids-ptr
                                                            avail-gids-buf)
                                          (munmap avail-gids-ptr (* +gid-t-size+ num-avail-gids1))))
                        (values
                          (make-iouser-mem eff-uids-ptr num-eff-uids1
                                           eff-gids-ptr num-eff-gids1)
                          (make-iouser-mem avail-uids-ptr num-avail-uids1
                                           avail-gids-ptr num-avail-gids1)))))))))
