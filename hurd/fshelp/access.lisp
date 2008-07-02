
(defmethod has-access-p ((stat stat) (user iouser) flag)
  (cond
    ((contains-uid user 0) t)
    ((and (empty-uids-p user)
	  (is-useunk-p stat))
     (has-perms stat flag 'unknown))
    ((is-owner-p stat user)
     (has-perms stat flag 'owner))
    ((contains-gid user (stat-get stat 'gid))
     (has-perms stat flag 'group))
    (t
      (has-perms stat flag 'others))))
