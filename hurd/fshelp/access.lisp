
(in-package :hurd)

(defmethod has-access-p ((stat stat) (user iouser) flag)
  "Verify if user can operate 'flag' on file identified by stat.
flag can be: 'read, 'write or 'exec."
  (cond
    ((contains-uid user 0) t)
    ((and (empty-uids-p user)
          (is-useunk-p stat))
     (has-perms-p stat flag 'unknown))
    ((is-owner-p stat user)
     (has-perms-p stat flag 'owner))
    ((contains-gid user (stat-get stat 'gid))
     (has-perms-p stat flag 'group))
    (t
      (has-perms-p stat flag 'others))))

