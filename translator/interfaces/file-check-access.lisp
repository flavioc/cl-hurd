
(in-package :hurd-translator)

(def-fs-interface :file-check-access ((file port)
                                      (flags :pointer))
  (with-lookup protid file
    (let ((ret (report-access *translator*
                              (get-node protid)
                              (get-user protid))))
      (cond
        ((listp ret)
         (setf (mem-ref flags 'open-flags) ret)
         t)
        (t ret)))))

