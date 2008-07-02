
(defun get-identity-port ()
  (let ((port (port-allocate :right-receive)))
    (if (not (port-valid port))
      (error "Invalid fsys identity port")
      port)))

(defclass translator ()
  ((identity-port :initform (get-identity-port)
				  :accessor identity-port)
   (port-bucket :initform nil
				:accessor port-bucket)
   (temporary-bucket :initform (make-hash-table)
					 :accessor temporary-bucket)
   (underlying-node :initform nil
					:reader underlying-node)
   (root-node :initform nil
			  :accessor root)
   (name :initform "cl-translator"
		 :accessor name)
   (version :initform (list 1 0 0)
			:accessor version)))

(defmethod initialize-instance :after ((translator translator) &key)
  (setf (port-bucket translator)
		(make-bucket (lambda (protid) nil)
					   ;(warn "protid release")))))
					   )))

(defmethod insert-temporary-data ((trans translator) key value)
  (setf (gethash (temporary-bucket trans) key) value))

(defmethod get-temporary-data ((trans translator) key)
  (with-cleanup (remove-temporary-data trans key)
				(gethash (temporary-bucket trans) key)))

(defmethod remove-temporary-data ((trans translator) key)
  (remhash (temporary-bucket trans) key))

(defmethod new-protid ((trans translator) user (open-node open-node))
  (add-port (port-bucket trans)
			(make-protid user open-node)))

(defun create-translator (&optional (base-class 'translator))
  (let ((translator (make-instance base-class)))
    (with-accessors ((id-port identity-port)) translator
      ; destroy identity port when translator goes away
      (finalize translator (lambda () (port-destroy id-port))))
    translator))

(defmethod setup ((trans translator) &optional (flags nil))
  (with-port-deallocate (bootstrap (get-bootstrap-port))
    (let ((port (add-new-port (port-bucket trans))))
      (with-port-deallocate (right (get-send-right port))
        (let ((file (fsys-startup bootstrap flags right :copy-send)))
		  (setf (slot-value trans 'underlying-node) file)
	  t)))))
