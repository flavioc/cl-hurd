
(def-io-interface :io-duplicate ((port port)
								 (newport port-pointer)
								 (newporttype :pointer))
   (with-lookup protid port
	  (let* ((new-user (make-iouser :old (get-user protid)))
			 (new (new-protid *translator* new-user
							  (open-node protid))))
		(setf (mem-ref newport 'port) (get-right new))
		(setf (mem-ref newporttype 'msg-type-name) :make-send)
		t)))
