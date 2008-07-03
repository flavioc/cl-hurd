
(defvar *ino-value* 1)

(defclass entry (node)
  ((name :initform nil
		 :initarg :name
		 :accessor name)
   (parent :initform nil
		   :initarg :parent
		   :accessor parent)))

(defmethod print-object ((node entry) stream)
  (format stream "#<entry name: ~s node: " (name node))
  (call-next-method)
  (format stream ">"))

(defclass dir-entry (entry)
   ((entries :initform (make-hash-table :test 'equal)
			 :accessor entries)))

(defun new-ino-val (stat)
  (setf (stat-get stat 'ino) (incf *ino-value*)))

(defun make-dir (name stat &optional (parent nil))
  (set-type stat 'dir)
  (new-ino-val stat)
  (setf (stat-get stat 'nlink) 1)
  (make-instance 'dir-entry :stat stat :name name :parent parent))

(defmethod add-entry ((dir dir-entry) (entry entry))
  (setf (gethash (name entry) (entries dir)) entry))

(defmethod setup-entry ((entry entry))
  (set-type (stat entry) 'reg)
  (new-ino-val (stat entry))
  (setf (stat-get (stat entry) 'nlink) 1))

(defun make-entry (name stat &optional (parent nil))
  (let ((obj (make-instance 'entry :stat stat :name name :parent parent)))
	(setup-entry obj)
	obj))

(defmethod dir-size ((dir dir-entry))
  (hash-table-count (entries dir)))

(defmethod get-entry ((dir dir-entry) (entry entry))
  (gethash (name entry) (entries dir)))

(defmethod get-entry ((dir dir-entry) (entry string))
  (gethash entry (entries dir)))

(defmethod remove-dir-entry ((dir dir-entry) (entry string))
  (remhash entry (entries dir)))
