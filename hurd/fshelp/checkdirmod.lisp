
(in-package :hurd)

(defmethod can-modify-dir-p ((dir stat) (user iouser))
  "Check if user can write on directory dir."
  (has-access-p dir user :write))

