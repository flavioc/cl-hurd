
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
         :accessor gids
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

(defun make-empty-iouser ()
  "Create an empty iouser."
  (make-instance 'iouser
                 :uids '()
                 :gids '()))

(defun make-iouser-mem (uids-ptr uids-len gids-ptr gids-len)
  "Create an iouser based on foreign pointers."
  (let ((obj (make-instance 'iouser
                            :uids (%idptr-to-list uids-ptr uids-len)
                            :gids (%idptr-to-list gids-ptr gids-len))))
    obj))

(defun make-iouser-root ()
  "Create the root iouser."
  (make-iouser :uids '(0) :gids '(0)))

(defmethod get-foreign-uids ((iouser iouser))
  (let* ((uids (uids iouser))
         (total (if (listp uids) (length uids) 0))
         (ptr (foreign-alloc 'uid-t :initial-contents uids)))
    (list ptr total)))

(defmethod get-foreign-gids ((iouser iouser))
  (let* ((gids (gids iouser))
         (total (if (listp gids) (length gids) 0))
         (ptr (foreign-alloc 'gid-t :initial-contents gids)))
    (list ptr total)))

(defmethod contains-uid-p ((iouser iouser) uid)
  "Does it contain 'uid' in UIDs?"
  (member uid (uids iouser)))

(defmethod contains-gid-p ((iouser iouser) gid)
  "Does it contain 'gid' in GIDs?"
  (member gid (gids iouser)))

(defmethod empty-uids-p ((iouser iouser))
  "Is the UIDs set empty?"
  (null (uids iouser)))

(defmethod empty-gids-p ((iouser iouser))
  "Is the GID's set empty?"
  (null (gids iouser)))

(defmethod count-uids ((user iouser))
  "Returns number of uids in user."
  (length (uids user)))

(defmethod count-gids ((user iouser))
  "Returns number of gids in user."
  (length (gids user)))

(defmethod add-uid ((user iouser) uid)
  "Add new uid to user."
  (push uid (uids user)))

(defmethod add-gid ((user iouser) gid)
  "Add new gid to user."
  (push gid (gids user)))

(defmethod clear-user ((user iouser))
  "Clear the UID+GID sets."
  (setf (uids user) (list))
  (setf (gids user) (list))
  user)

(defmethod restrict-iouser ((user1 iouser) (user2 iouser))
  "Returns an intersection of user1 and user2, except when user1 is root and then we return user2."
  (cond
    ((contains-uid user1 0) user2) ; root user
    (t
      (make-iouser :uids (intersection (uids user1)
                                       (uids user2))
                   :gids (intersection (gids user1)
                                       (gids user2))))))

(defmethod uid-setid ((eff-user iouser) (avail-user iouser) id)
  "Does the same thing as idvec_setid only for UID set.
Set the first element of UID's eff-user set as id.
When the avail-user is not empty, set the second element as id."
  (let ((secure (and (not (contains-uid-p eff-user id))
                     (not (contains-uid-p avail-user id)))))
    (cond
      ((empty-uids-p eff-user) (add-uid eff-user id))
      (t
        (add-uid avail-user id)
        (setf (first (uids eff-user)) id)))
    (unless (empty-uids-p avail-user)
      (cond
        ((> (count-uids avail-user) 1)
         (setf (second (uids avail-user)) id))
        (t (add-uid avail-user id))))
    secure))

(defmethod gid-setid ((eff-user iouser) (avail-user iouser) id)
  "Does the same thing as idvec_setid, only for the GID set."
  (let ((secure (and (not (contains-gid-p eff-user id))
                     (not (contains-gid-p avail-user id)))))
    (cond
      ((empty-gids-p eff-user) (add-gid eff-user id))
      (t
        (add-gid avail-user id)
        (setf (first (gids eff-user)) id)))
    (unless (empty-gids-p avail-user)
      (cond
        ((> (count-gids avail-user) 1)
         (setf (second (gids avail-user)) id))
        (t (add-gid avail-user id))))
    secure))

(defmethod print-object ((iouser iouser) stream)
  "Print an iouser object."
  (format stream
          "#<iouser: uids=~s gids=~s>"
          (uids iouser) (gids iouser)))
