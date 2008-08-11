
(in-package :hurd-translator)

(defun make-node-dirent (name node)
  "Creates a new dirent object based on a standard node."
  (make-dirent name
               (stat-get (stat node) 'st-ino)
               (stat-get (stat node) 'st-type)))

