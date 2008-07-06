
(in-package :hurd)

(defmethod can-modify-dir-p ((dir stat) (user iouser))
  "Check if user can write on directory dir."
  (has-access-p dir user 'write))

(defmethod can-modify-file-in-dir-p ((dir stat) (user iouser) (file stat))
  "Check if user can modify a file in dir."
  (when (can-modify-dir-p dir user)
    (not (and (is-vtx-p dir)
              (not (is-owner-p dir user))
              (not (is-owner-p file user))))))

