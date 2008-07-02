
(defclass iouser ()
  ((uids :initform (list)
	 :initarg :uids
	 :accessor uids
	 :documentation "List of uids")
   (gids :initform (list)
	 :initarg :gids
	 :reader gids
	 :documentation "List of gids"))
  (:documentation "Groups list of uids and gids representing an I/O user"))

(defun make-iouser (&key (uids nil) (gids nil) (old nil))
  "Create an iouser based on lists or atoms of uids and gids"
  (if old
	(make-instance 'iouser
				   :uids (uids old)
				   :gids (gids old))
	(make-instance 'iouser
				   :uids (if (listp uids) uids (list uids))
				   :gids (if (listp gids) gids (list gids)))))

(defun make-iouser-mem (uids-ptr uids-len gids-ptr gids-len)
  "Create an iouser based on foreign pointers"
  (let ((obj (make-instance 'iouser
			    :uids (idptr-to-list uids-ptr uids-len)
			    :gids (idptr-to-list gids-ptr gids-len))))
;    (free-ptr uids-ptr uids-len)
 ;   (free-ptr gids-ptr gids-len)
    obj))

(defmethod contains-uid ((iouser iouser) uid)
  (member uid (uids iouser)))

(defmethod contains-gid ((iouser iouser) gid)
  (member gid (gids iouser)))

(defmethod empty-uids-p ((iouser iouser))
  (null (uids iouser)))

(defmethod empty-gids-p ((iouser iouser))
  (null (gids iouser)))

(defmethod restrict-iouser ((user1 iouser) (user2 iouser))
  (cond
    ((contains-uid user1 0) user2) ; root user
    (t
      (make-iouser :uids (intersection (uids user1)
				       (uids user2))
		   :gids (intersection (gids user1)
				       (gids user2))))))
(defmethod print-object ((iouser iouser) stream)
  (format stream "#<iouser: uids -> ~s ; gids -> ~s>" (uids iouser) (gids iouser)))
