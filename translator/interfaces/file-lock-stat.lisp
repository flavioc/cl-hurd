
(in-package :hurd-translator)

(def-fs-interface :file-lock-stat ((file port)
                                   (status :pointer)
                                   (otherstatus :pointer))
  (with-lookup protid file
    (let ((status (lock-status (open-node protid))))
      (setf (mem-ref status 'lock-flags) status
            (mem-ref otherstatus 'lock-flags) status)
      t)))
