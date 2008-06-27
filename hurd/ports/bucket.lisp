
(defclass port-bucket ()
  ((port-set :initform (port-allocate :right-port-set)
	     :accessor port-set
	     :documentation "A port set containing all the ports in the bucket")
   (hash :initform (make-hash-table)
	 :accessor table
	 :documentation "Contains the ports in this bucket")))

(defmethod cleanup ((bucket port-bucket))
  (with-accessors ((port port-set)) bucket
    (port-destroy port)))

(defmethod add-port ((bucket port-bucket))
  (let ((port (make-instance 'port-info)))
    (move-receive-right bucket (port-right port))
    (setf (gethash (port-right port) (table bucket)) port)
    port))

(defmethod move-receive-right ((bucket port-bucket) right)
  (port-move-member right
		    (port-set bucket)))

(defmethod has-port ((bucket port-bucket) port)
  (with-accessors ((table table)) bucket
    (multiple-value-bind (val found) (gethash port table)
      found)))

(defun make-bucket ()
  (make-instance 'port-bucket))
