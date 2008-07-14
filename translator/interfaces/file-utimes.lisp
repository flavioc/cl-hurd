
(in-package :hurd-translator)

(def-fs-interface :file-utimes ((file port)
                                (atimein time-value-t)
                                (mtimein time-value-t))
  (with-lookup protid file
    (file-utimes *translator*
                 (get-node protid)
                 (get-user protid)
                 atimein
                 mtimein)))

