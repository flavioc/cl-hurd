
(in-package :hurd-translator)

;; Sync a filesystem.
(def-fsys-interface :fsys-syncfs ((control port)
                                  (reply port)
                                  (reply-type msg-type-name)
                                  (wait :boolean)
                                  (children :boolean))
  (declare (ignore reply reply-type))
  (when (port-exists-p control)
    (let ((root-user (make-iouser-root)))
      (file-syncfs *translator*
                   root-user
                   wait
                   children)
      t)))

