
(in-package :hurd-translator)

(def-io-interface :io-seek ((io port)
                            (offset off-t)
                            (whence seek-type)
                            (newoffset :pointer))
  (with-lookup protid io
    (case whence
      (:seek-cur
        (incf offset (file-offset (open-node protid))))
      (:seek-end
        (incf offset (stat-get (stat (get-node protid)) 'st-size))))
    (cond
      ((and
         (>= offset 0)
         (<= offset (stat-get (stat (get-node protid)) 'st-size)))
       (setf (mem-ref newoffset 'off-t) offset
             (file-offset (open-node protid)) offset)
       t)
      (t :invalid-argument))))

