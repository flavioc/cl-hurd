
(def-notify-interface :do-mach-notify-no-senders ((port port)
												  (count port-mscount))
  ;; lookup port on the *all-ports* table
  ;; with success we get a list with a port
  ;; and the respective bucket
  (let ((port-data (gethash port *all-ports*)))
	(when (listp port-data)
	  (let ((port-info (first port-data))
			(bucket (second port-data)))
		(when (has-send-rights port-info)
		  ;; remove it from the bucket
		  (remove-port bucket port-info)
		  ;; also from the *all-ports* table
		  (remhash port *all-ports*))))))
