
(in-package :hurd-translator)

(def-fs-interface :file-check-access ((file port)
                                      (flags :pointer))
  (with-lookup protid file
    (setf (mem-ref flags 'open-flags)
          (report-access *translator*
                         (get-node protid)
                         (get-user protid)))
      t))

