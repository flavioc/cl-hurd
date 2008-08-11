
(in-package :hurd)

(defcfun ("auth_server_authenticate" %auth-server-authenticate)
  err
  (auth auth-t)
  (rendezvous port)
  (rendezvous-type msg-type-name)
  (newport port)
  (newport-type msg-type-name)
  (euids :pointer) ; idarray_t*
  (euids-count :pointer) ; msg-type-number
  (auids :pointer)
  (auids-count :pointer)
  (egids :pointer)
  (egids-count :pointer)
  (agids :pointer)
  (agis-count :pointer))

(defun user-reauth (auth-server rend-port new-right permit-failure-p)
  "Makes an user reauthentication with 'auth-server'."
  (let* ((gubuf (foreign-alloc 'uid-t :count 20))
         (ggbuf (foreign-alloc 'gid-t :count 20))
         (aubuf (foreign-alloc 'uid-t :count 20))
         (agbuf (foreign-alloc 'gid-t :count 20))
         (gen-uids (%new-ptr))
         (gen-gids (%new-ptr))
         (aux-uids (%new-ptr))
         (aux-gids (%new-ptr))
         (gen-uids-len (%new-unsigned 20))
         (gen-gids-len (%new-unsigned 20))
         (aux-uids-len (%new-unsigned 20))
         (aux-gids-len (%new-unsigned 20)))

    (setf (mem-ref gen-uids :pointer) gubuf
          (mem-ref gen-gids :pointer) ggbuf
          (mem-ref aux-uids :pointer) aubuf
          (mem-ref aux-gids :pointer) agbuf)

    (with-cleanup (free-memory-list (list
                                       gubuf ggbuf aubuf agbuf
                                       gen-uids gen-gids aux-uids aux-gids
                                       gen-uids-len gen-gids-len aux-uids-len aux-gids-len))
      (let ((err (%auth-server-authenticate
                   auth-server
                   rend-port
                   :copy-send
                   new-right
                   :copy-send
                   gen-uids gen-uids-len
                   aux-uids aux-uids-len
                   gen-gids gen-gids-len
                   aux-gids aux-gids-len)))
        (let ((uids-len (mem-ref gen-uids-len :unsigned-int))
              (gids-len (mem-ref gen-gids-len :unsigned-int))
              (uids-ptr (mem-ref gen-uids :pointer))
              (gids-ptr (mem-ref gen-gids :pointer))
              (auids-len (mem-ref aux-uids-len :unsigned-int))
              (agids-len (mem-ref aux-gids-len :unsigned-int))
              (auids-ptr (mem-ref aux-uids :pointer))
              (agids-ptr (mem-ref aux-gids :pointer)))
          (with-cleanup (progn
                          (unless (pointer-eq gubuf uids-ptr)
                            (munmap uids-ptr (* +uid-t-size+ uids-len)))
                          (unless (pointer-eq ggbuf gids-ptr)
                            (munmap gids-ptr (* +gid-t-size+ gids-len)))
                          (unless (pointer-eq aubuf auids-ptr)
                            (munmap auids-ptr (* +uid-t-size+ auids-len)))
                          (unless (pointer-eq agbuf agids-ptr)
                            (munmap agids-ptr (* +gid-t-size+ agids-len))))
            (cond
              ((eq err t)
               (make-iouser-mem uids-ptr uids-len gids-ptr gids-len))
              (t
                (if permit-failure-p
                  (make-empty-iouser)
                  nil)))))))))

