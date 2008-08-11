
(in-package :hurd-translator)

(def-io-interface :io-seek ((io port)
                            (offset off-t)
                            (whence seek-type)
                            (newoffset :pointer))
  (with-lookup protid io
    (let ((node (get-node protid))
          (user (get-user protid))
          (open (open-node protid)))
      (case whence
        (:seek-cur
          (incf offset (file-offset open)))
        (:seek-end
          (incf offset (stat-get (stat node) 'st-size))))
      (cond
        ((>= offset 0)
         (setf (mem-ref newoffset 'loff-t) offset
               (file-offset open) offset)
         ; Warn user of pointer change
         (report-seek *translator* node user offset)
         t)
        (t :invalid-argument)))))

