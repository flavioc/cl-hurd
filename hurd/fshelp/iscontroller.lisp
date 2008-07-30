
(in-package :hurd)

(defmethod is-controller-p ((stat stat) (user iouser))
  "Check if 'user' controls file 'stat'."
  (or (contains-uid-p user 0)
      (contains-uid-p user (stat-get stat 'st-uid))
      (contains-uid-p user (geteuid))))

