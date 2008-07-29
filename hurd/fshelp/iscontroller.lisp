
(in-package :hurd)

(defmethod is-controller-p ((stat stat) (user iouser))
  "Check if 'user' controls file 'stat'."
  (or (contains-uid user 0)
      (contains-uid user (stat-get stat 'st-uid))
      (contains-uid user (geteuid))))

