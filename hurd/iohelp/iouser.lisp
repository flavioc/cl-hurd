
(in-package :hurd)

;;
;; This implements the IO user abstraction.
;; An iouser is represented by a set of UID's and GID's.
;;

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
  "Create an iouser based on lists or atoms of uids and gids. Can also copy other iouser objects."
  (if old
    ; Just make a copy.
    (make-instance 'iouser
                   :uids (uids old)
                   :gids (gids old))
    ; Use the indicated lists.
    (make-instance 'iouser
                   :uids (if (listp uids) uids (list uids))
                   :gids (if (listp gids) gids (list gids)))))

(defun make-iouser-mem (uids-ptr uids-len gids-ptr gids-len)
  "Create an iouser based on foreign pointers."
  (let ((obj (make-instance 'iouser
                            :uids (%idptr-to-list uids-ptr uids-len)
                            :gids (%idptr-to-list gids-ptr gids-len))))
    ;(%free-ptr uids-ptr uids-len)
    ;(%free-ptr gids-ptr gids-len)
    obj))

(defun make-iouser-root ()
  "Create the root iouser."
  (make-iouser :uids '(0) :gids '(0)))

(defmethod contains-uid ((iouser iouser) uid)
  "Does it contain 'uid' in UIDs?"
  (member uid (uids iouser)))

(defmethod contains-gid ((iouser iouser) gid)
  "Does it contain 'gid' in GIDs?"
  (member gid (gids iouser)))

(defmethod empty-uids-p ((iouser iouser))
  "Is the UIDs set empty?"
  (null (uids iouser)))

(defmethod empty-gids-p ((iouser iouser))
  "Is the GID's set empty?"
  (null (gids iouser)))

(defmethod restrict-iouser ((user1 iouser) (user2 iouser))
  "Returns an intersection of user1 and user2, except when user1 is root and then we return user2."
  (cond
    ((contains-uid user1 0) user2) ; root user
    (t
      (make-iouser :uids (intersection (uids user1)
                                       (uids user2))
                   :gids (intersection (gids user1)
                                       (gids user2))))))

(defmethod print-object ((iouser iouser) stream)
  "Print an iouser object."
  (format stream
          "#<iouser: uids -> ~s ; gids -> ~s>"
          (uids iouser) (gids iouser)))
