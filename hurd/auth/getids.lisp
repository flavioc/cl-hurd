
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
  (let ((eff-uids (foreign-alloc :pointer))
        (num-eff-uids (foreign-alloc :unsigned-int :initial-element 0))
        (avail-uids (foreign-alloc :pointer))
        (num-avail-uids (foreign-alloc :unsigned-int :initial-element 0))
        (eff-gids (foreign-alloc :pointer))
        (num-eff-gids (foreign-alloc :unsigned-int :initial-element 0))
        (avail-gids (foreign-alloc :pointer))
        (num-avail-gids (foreign-alloc :unsigned-int :initial-element 0)))
    (with-cleanup (free-memory-list
                    (list eff-uids
                          num-eff-uids
                          eff-gids
                          num-eff-uids
                          avail-gids
                          num-avail-gids
                          eff-gids
                          num-eff-gids))
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
                            (num-eff-uids (mem-ref num-eff-uids :unsigned-int))
                            (avail-uids-ptr (mem-ref avail-uids :pointer))
                            (num-avail-uids (mem-ref num-avail-uids :unsigned-int))
                            (eff-gids-ptr (mem-ref eff-gids :pointer))
                            (num-eff-gids (mem-ref num-eff-gids :unsigned-int))
                            (avail-gids-ptr (mem-ref avail-gids :pointer))
                            (num-avail-gids (mem-ref num-avail-gids :unsigned-int)))
                        (with-cleanup (progn
                                        (munmap eff-uids-ptr (* +uid-t-size+ num-eff-uids))
                                        (munmap avail-uids-ptr (* +uid-t-size+ num-avail-uids))
                                        (munmap eff-gids-ptr (* +gid-t-size+ num-eff-gids))
                                        (munmap avail-gids-ptr (* +gid-t-size+ num-avail-gids)))
                        (values
                          (make-iouser-mem eff-uids-ptr num-eff-uids
                                           eff-gids-ptr num-eff-gids)
                          (make-iouser-mem avail-uids-ptr num-avail-uids
                                           avail-gids-ptr num-avail-gids)))))))))
