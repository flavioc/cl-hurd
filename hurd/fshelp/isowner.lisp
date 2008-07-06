
(in-package :hurd)

(defmethod is-owner-p ((stat stat) (user iouser))
  "Check if 'user' owns file 'stat'."
  (or (contains-uid user 0)
      (contains-uid user (stat-get stat 'uid))
      (and (contains-gid user (stat-get stat 'gid))
           (contains-uid user (stat-get stat 'gid)))))

