
(defclass node ()
   ((owner :initform 0
		   :initarg :owner
		   :accessor owner)
	(stat :initform (make-stat)
		  :initarg :stat
		  :accessor stat)
	(references :initform 1
				:accessor references)
	(box :initform nil
		 :accessor box)))

(defmethod initialize-instance :after ((node node) &key)
  (setf (box node) (make-transbox node)))

(defmethod print-object ((node node) stream)
  (format stream "#<node ref: ~s>" (references node)))
