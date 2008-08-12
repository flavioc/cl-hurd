
(in-package :hurd-translator)

(def-fs-interface :file-lock-stat ((file port)
                                   (status :pointer)
                                   (otherstatus :pointer))
  (with-lookup protid file
    (let ((status-flags (lock-status (open-node protid))))
      (setf (mem-ref status 'lock-flags) status-flags
            (mem-ref otherstatus 'lock-flags) status-flags)
      t)))

