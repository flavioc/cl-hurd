
(defvar *all-ports* (make-hash-table))

(defclass port-bucket ()
  ((port-set :initform (port-allocate :right-port-set)
	     :accessor port-set
	     :documentation "A port set containing all the ports in the bucket")
   (cleanup :initform nil
			:initarg :cleanup
			:accessor cleanup-function
			:documentation "To be run when a port is not used anymore")
   (hash :initform (make-hash-table)
		 :accessor table
		 :documentation "Contains the ports in this bucket")))

(defmethod cleanup ((bucket port-bucket))
  (with-accessors ((port port-set)) bucket
    (port-destroy port)))

(defmethod add-new-port ((bucket port-bucket))
  (let ((port (make-instance 'port-info)))
    (add-port bucket port)))

(defmethod add-port ((bucket port-bucket) (port port-info))
  (move-receive-right bucket (port-right port))
  (setf (gethash (port-right port) (table bucket)) port)
  ;; also add this port to *all-ports* so we can look
  ;; it up when a no-senders notification arrives
  (setf (gethash (port-right port) *all-ports*)
		(list port bucket))
  port)

(defmethod move-receive-right ((bucket port-bucket) right)
  (port-move-member right
		    (port-set bucket)))

(defmethod has-port ((bucket port-bucket) port)
  (with-accessors ((table table)) bucket
    (multiple-value-bind (val found) (gethash port table)
      found)))

(defmethod lookup-port ((bucket port-bucket) port)
  (with-accessors ((table table)) bucket
	(gethash port table)))

(defmethod remove-port ((bucket port-bucket) port)
  (with-accessors ((cleanup-fun cleanup-function)) bucket
	(remhash (port-right port) (table bucket))
	(set-send-rights port nil)
	(if cleanup-fun
	  (funcall cleanup-fun port))
	(port-cleanup port)))

(defun make-bucket (cleanup)
  (make-instance 'port-bucket :cleanup cleanup))
