
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

(defun make-iouser (&key (uids nil) (gids nil))
  "Create an iouser based on lists or atoms of uids and gids"
  (make-instance 'iouser
		 :uids (if (listp uids) uids (list uids))
		 :gids (if (listp gids) gids (list gids))))

(defun make-iouser-mem (uids-ptr uids-len gids-ptr gids-len)
  "Create an iouser based on foreign pointers"
  (let ((obj (make-instance 'iouser
			    :uids (idptr-to-list uids-ptr uids-len)
			    :gids (idptr-to-list gids-ptr gids-len))))
    (free-ptr uids-ptr uids-len)
    (free-ptr gids-ptr gids-len)
    obj))

(defun contains-uid ((iouser iouser) uid)
  (member uid (uids iouser)))

(defmethod restrict-iouser ((user1 iouser) (user2 iouser))
  (cond
    ((contains-uid user1 0) user2) ; root user
    (t
      (make-iouser :uids (intersection (uids user1)
				       (uids user2))
		   :gids (intersection (gids user1)
				       (gids user2))))))

