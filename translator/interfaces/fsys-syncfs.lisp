
(in-package :hurd-translator)

;; Sync a filesystem.
(def-fsys-interface :fsys-syncfs ((control port)
                                  (reply port)
                                  (reply-type msg-type-name)
                                  (wait :boolean)
                                  (children :boolean))
  (declare (ignore reply reply-type children))
  (when (port-exists-p control)
    (let ((root-user (make-iouser-root)))
      (if (sync-fs *translator*
                   root-user
                   wait)
        t
        nil))))

