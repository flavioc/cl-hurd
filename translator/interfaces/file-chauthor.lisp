
(in-package :hurd-translator)

(def-fs-interface :file-chauthor ((port port)
								  (author uid-t))
  (with-lookup protid port
    (when (allow-author-change-p *translator*
                                 (get-node protid)
                                 (get-user protid)
                                 author)
      (setf (stat-get (stat (get-node protid)) 'st-author) author))
    t))
