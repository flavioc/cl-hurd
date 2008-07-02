
(defclass dirent ()
  ((name :initarg :name
		 :accessor name)
   (node :initarg :node
		 :accessor node)))

(defun make-dirent (name node)
  (make-instance 'dirent :name name :node node))
