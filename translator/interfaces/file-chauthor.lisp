
(def-fs-interface :file-chauthor ((port port)
								  (author uid-t))
  (with-lookup protid port
	 (when (allow-author-change *translator*
								(get-node protid)
								(get-user protid)
								author)
	   (setf (stat-get (stat (get-node protid)) 'author) author))
	 t))
