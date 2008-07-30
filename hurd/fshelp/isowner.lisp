
(in-package :hurd)

(defmethod is-owner-p ((stat stat) (user iouser))
  "Check if 'user' owns file 'stat'."
  (or (contains-uid-p user 0)
      (contains-uid-p user (stat-get stat 'st-uid))
      (and (contains-gid-p user (stat-get stat 'st-gid))
           (contains-uid-p user (stat-get stat 'st-gid)))))

