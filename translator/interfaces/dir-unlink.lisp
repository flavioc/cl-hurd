
(def-fs-interface :dir-unlink ((port port)
							   (name :string))
   (with-lookup protid port
	  (if (remove-entry *translator*
						(get-node protid)
						(get-user protid)
						name
						nil) ;; not a directory
		t
		:not-permitted)))
