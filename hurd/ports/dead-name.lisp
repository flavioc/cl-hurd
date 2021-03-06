
(in-package :hurd)

;; This is run when we get a dead name notification
(def-notify-interface :do-mach-notify-dead-name
                      ((notify port) (name port))
  (let ((port-data (gethash notify *all-ports*)))
    (when (listp port-data)
      (let ((port-info (first port-data))
            (bucket (second port-data)))
      (remhash notify *all-ports*)
      (when bucket
        (bucket-remove-port bucket port-info nil))
      (port-deallocate name)))))
