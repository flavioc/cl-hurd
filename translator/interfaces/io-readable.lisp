
(def-io-interface :io-readable ((port port)
								(amount :pointer))
  (with-lookup protid port
	(warn "io-readable!!!")
	(block io-readable
		   (let ((open (open-node protid))
				 (node (get-node protid)))
			 (unless (is (flags open) 'read)
			   (return-from io-readable :invalid-argument))
			 (setf (mem-ref amount 'msg-type-number)
				   (max 0
						(- (stat-get (stat node) 'size)
						   (file-offset open))))
			 t))))

