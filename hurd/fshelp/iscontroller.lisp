
(defmethod is-controller ((stat stat) (user iouser))
  (or (contains-uid user 0)
      (contains-uid user (stat-get stat 'uid))
      (contains-uid user (geteuid))))

