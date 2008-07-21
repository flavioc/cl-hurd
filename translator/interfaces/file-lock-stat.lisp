
(in-package :hurd-translator)

(def-fs-interface :file-lock-stat ((file port)
                                   (status :pointer)
                                   (otherstatus :pointer))
  (with-lookup protid file
    (setf (mem-ref status 'lock-flags) (lock-status (open-node protid))
          (mem-ref otherstatus :int) 0)
    t))
