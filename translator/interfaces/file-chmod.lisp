
(def-fs-interface :file-chmod ((file port)
							   (mode mode-t))
  (with-lookup protid file
	; we only want permission bits
	; disable those bits that don't make much sense here
	(set-spare mode nil)
	(set-types mode nil)
	(set-trans mode nil)
	(file-chmod *translator*
				(get-node protid)
				(get-user protid)
				mode)))
