
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

(defun %new-ptr ()
  (foreign-alloc :pointer))

(defun %new-unsigned (n)
  (foreign-alloc :unsigned-int :initial-element n))

(defun user-reauth (auth-server rend-port new-right permit-failure-p)
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
          ;                (warn "testing gubuf")
                          (unless (pointer-eq gubuf uids-ptr)
         ;                   (warn "cleanup gubuf")
                            (munmap uids-ptr (* +uid-t-size+ uids-len)))
        ;                  (warn "testing ggbuf")
                          (unless (pointer-eq ggbuf gids-ptr)
       ;                     (warn "cleanup ggbuf")
                            (munmap gids-ptr (* +gid-t-size+ gids-len)))
      ;                    (warn "testing aubuf")
                          (unless (pointer-eq aubuf auids-ptr)
     ;                       (warn "cleanup aubuf")
                            (munmap auids-ptr (* +uid-t-size+ auids-len)))
    ;                      (warn "testing agbuf")
                          (unless (pointer-eq agbuf agids-ptr)
   ;                         (warn "cleanup agbuf")
                            (munmap agids-ptr (* +gid-t-size+ agids-len))))
            (cond
              ((eq err t)
  ;             (warn "ok... creating iouser.")
 ;              (warn "uids-ptr ~s" uids-ptr)
               (make-iouser-mem uids-ptr uids-len gids-ptr gids-len))
              (t
;                (warn "fail..")
                (if permit-failure-p
                  (make-empty-iouser)
                  nil)))))))))

