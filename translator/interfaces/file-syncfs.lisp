
(def-fs-interface :file-syncfs ((port port)
								(wait :boolean)
								(do-children :boolean))
  (with-lookup protid port
	(if (file-syncfs *translator*
					 (get-user protid)
					 wait
					 do-children)
	  t
	  nil)))

