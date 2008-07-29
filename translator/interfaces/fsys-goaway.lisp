
(in-package :hurd-translator)

(defun %shutdown (flags)
  (when (and
          (flag-is-p flags :unlink)
          (is-dir-p (stat (root *translator*))))
    (return-from %shutdown :resource-busy))
  (when (flag-is-p flags :recurse)
    (bucket-iterate
      (port-bucket *translator*)
      (lambda (port)
        (when (typep port 'protid)
          (let ((node (get-node port)))
            (when (box-active-p (box node))
              (with-port-deallocate (control (box-fetch-control (box node)))
                ; We will catch errors later on
                (warn "killing a translator..")
                (when (fsys-goaway control flags)
                  (box-set-active (box node) nil nil))))))))
    ; Wait for no-senders port notifications
    (wait :miliseconds 500))
  (bucket-iterate (port-bucket *translator*)
                  (lambda (port)
                    (when (typep port 'protid)
                      (warn "node ~s still active" (get-node port)))))
  (when (and
          (not (flag-is-p flags :force))
          (plusp (bucket-count-type
                   (port-bucket *translator*) 'protid)))
    (return-from %shutdown :resource-busy))
  (unless (flag-is-p flags :nosync)
    (sync-fs *translator* (make-iouser-root) t t))
  (shutdown *translator*)
  t)

;;
;; Run the fsys-goaway callback terminating the translator.
;;
(def-fsys-interface :fsys-goaway ((control port)
                                  (reply port)
                                  (reply-type msg-type-name)
                                  (flags fsys-goaway-flags))
  (when (port-exists-p control)
    (let ((shut-ret (%shutdown flags)))
      (cond
        ((eq t shut-ret)
         (fsys-goaway-reply reply reply-type t)
         (exit 0))
        (t
          shut-ret)))))

