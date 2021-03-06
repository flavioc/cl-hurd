
(in-package :hurd-translator)

(def-io-interface :io-readable ((port port)
                                (amount :pointer))
  (with-lookup protid port
    (block io-readable
           (let ((open (open-node protid))
                 (node (get-node protid)))
             (unless (flag-is-p (flags open) :read)
               (return-from io-readable :invalid-argument))
             (setf (mem-ref amount 'vm-size)
                   (max 0
                        (- (stat-get (stat node) 'st-size)
                           (file-offset open))))
             t))))

