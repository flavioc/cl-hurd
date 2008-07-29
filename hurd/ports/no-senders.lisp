
(in-package :hurd)

;; This is run when we get a no senders notification
(def-notify-interface :do-mach-notify-no-senders
                      ((port port) (count port-mscount))
  ;; Lookup port on the *all-ports* table
  ;; with success we get a list with a port
  ;; and the respective bucket
  (declare (ignore count))
  (let ((port-data (gethash port *all-ports*)))
    (when (listp port-data)
      (let ((port-info (first port-data))
            (bucket (second port-data)))
        ;; Remove it from the bucket
        (bucket-remove-port bucket port-info)
        ;; Also from the *all-ports* table
        (remhash port *all-ports*)))))
