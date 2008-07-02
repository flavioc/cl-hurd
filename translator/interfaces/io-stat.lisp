
(def-io-interface :io-stat ((io port)
							(stat-info stat-t))
  (with-lookup protid io
	 (copy-stat-struct stat-info (get-stat protid))
	 (set-root stat-info nil)
	 (set-active-trans stat-info nil)
	 (set-active-trans stat-info
					   (box-translated-p (get-box protid)))
	 (set-root stat-info
			   (or (eq (get-node protid) (get-shadow-root protid))
				   (eq (get-node protid) (root *translator*))))
	 t))

