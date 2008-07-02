
(def-fs-interface :dir-rmdir ((port port)
							  (name :string))
  (with-lookup protid port
	 (if (remove-entry *translator*
					   (get-node protid)
					   (get-user protid)
					   name
					   t)
	   t
	   :not-permitted)))
