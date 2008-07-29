
(in-package :hurd-translator)

(def-io-interface :io-seek ((io port)
                            (offset loff-t)
                            (whence seek-type)
                            (newoffset :pointer))
  (with-lookup protid io
    (case whence
      (:seek-cur
        (incf offset (file-offset (open-node protid))))
      (:seek-end
        (incf offset (stat-get (stat (get-node protid)) 'st-size))))
    (cond
      ((plusp offset)
       (setf (mem-ref newoffset 'loff-t) offset
             (file-offset (open-node protid)) offset)
       t)
      (t
        :invalid-argument))))

