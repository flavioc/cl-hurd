
(in-package :hurd-translator)

(defun %shutdown (flags)
  (when (and
          (flag-is-p flags :unlink)
          (is-dir-p (stat (root *translator*))))
    (return-from %shutdown :resource-busy))
  (when (flag-is-p flags :recurse)
    (bucket-iterate (port-bucket *translator*)
                    (lambda (port)
                      (when (typep port 'protid)
                        (let ((node (get-node port)))
                          (when (box-active-p (box node))
                            (with-port-deallocate (control (box-fetch-control (box node)))
                              ; We will catch errors later on
                              (fsys-goaway control flags)))))))
    (wait :miliseconds 500))
  (when (and
          (not (flag-is-p flags :force))
          (plusp (bucket-count-type (port-bucket *translator*) 'protid)))
    (return-from %shutdown :resource-busy))
  (unless (flag-is-p flags :nosync)
    (file-syncfs *translator* (make-iouser-root) t t))
  (shutdown *translator*))

;;
;; Run the fsys-goaway callback terminating the translator.
;;
(def-fsys-interface :fsys-goaway ((control port)
                                  (reply port)
                                  (reply-type msg-type-name)
                                  (flags fsys-goaway-flags))
  (when (port-exists-p control)
    (warn "flags ~s" flags)
    (let ((shut-ret (%shutdown flags)))
      (cond
        ((eq t shut-ret)
         (fsys-goaway-reply reply reply-type t)
         (exit 0))
        (t
          shut-ret)))))

