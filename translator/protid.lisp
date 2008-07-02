
(defclass protid (port-info)
  ((user :initform nil
		 :initarg :user)
   (open-node :initform nil
			:initarg :open-node
			:accessor open-node)))

(defun make-protid (user open-node)
  (make-instance 'protid
				 :user user
				 :open-node open-node))

(defmethod get-stat ((protid protid))
  (stat (get-node protid)))

(defmethod get-box ((protid protid))
  (box (get-node protid)))

(defmethod get-node ((protid protid))
  (refers (open-node protid)))

(defmethod get-user ((protid protid))
  (slot-value protid 'user))

(defmethod get-shadow-root ((protid protid))
  (shadow-root (open-node protid)))

(defmethod get-shadow-root-parent ((protid protid))
  (shadow-root-parent (open-node protid)))

(defmethod get-root-parent ((protid protid))
  (root-parent (open-node protid)))

(defmethod get-open-flags ((protid protid))
  (flags (open-node protid)))
