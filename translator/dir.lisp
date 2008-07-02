
(defvar *ino-value* 5)

(defclass entry (node)
  ((name :initform nil
		 :initarg :name
		 :accessor name)
   (parent :initform nil
		   :initarg :parent
		   :accessor parent)))

(defclass dir (entry)
   ((entries :initform (make-hash-table :test 'equal)
			 :accessor entries)))

(defun make-dir (name stat &optional (parent nil))
  (set-type stat 'dir)
  (incf *ino-value*)
  (setf (stat-get stat 'ino) *ino-value*)
  (setf (stat-get stat 'nlink) 1)
  (make-instance 'dir :stat stat :name name :parent parent))

(defmethod add-entry ((dir dir) (entry entry))
  (setf (gethash (name entry) (entries dir)) entry))

(defun make-entry (name stat &optional (parent nil))
  (set-type stat 'reg)
  (incf *ino-value*)
  (setf (stat-get stat 'ino) *ino-value*)
  (setf (stat-get stat 'nlink) 1)
  (make-instance 'entry :stat stat :name name :parent parent))

(defmethod dir-size ((dir dir))
  (hash-table-count (entries dir)))

(defmethod get-entry ((dir dir) (entry entry))
  (gethash (name entry) (entries dir)))

(defmethod get-entry ((dir dir) (entry string))
  (gethash entry (entries dir)))

(defmethod remove-dir-entry ((dir dir) (entry string))
  (remhash entry (entries dir)))
