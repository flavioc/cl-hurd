
(in-package :hurd)

(defmethod has-access-p ((stat stat) (user iouser) flag)
  "Verify if user can operate 'flag' on file identified by stat.
flag can be: :read, :write or :exec."
  (cond
    ((contains-uid-p user 0)
     (if (eq flag :exec)
       (has-perms-p stat :exec)
       t))
    ((and (empty-uids-p user)
          (is-useunk-p stat))
     (has-perms-p stat flag :unknown))
    ((is-owner-p stat user)
     (has-perms-p stat flag :owner))
    ((contains-gid-p user (stat-get stat 'st-gid))
     (has-perms-p stat flag :group))
    (t
      (has-perms-p stat flag :others))))

