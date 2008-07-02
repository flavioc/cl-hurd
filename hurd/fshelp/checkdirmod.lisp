
(defmethod can-modify-dir-p ((dir stat) (user iouser))
  (has-access-p dir user 'write))

(defmethod can-modify-file-in-dir-p ((dir stat) (user iouser) (file stat))
  (when (can-modify-dir-p dir user)
    (not (and (is-vtx-p dir)
	      (not (is-owner-p dir user))
	      (not (is-owner-p file user))))))

