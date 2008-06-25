
(defun get-identity-port ()
  (let ((port (port-allocate :right-receive)))
    (if (not (port-valid port))
      (error "Invalid fsys identity port")
      port)))

(defun create-control-class ()
  (ports-create-class))

(defun create-protid-class ()
  (ports-create-class))

(defclass translator ()
  ((identity-port :initform (get-identity-port)
		  :accessor identity-port)
   (protid-class :initform (create-protid-class)
				 :accessor protid-class)
   (control-class :initform (create-control-class)
			  :accessor control-class)
   (port-bucket :initform (ports-create-bucket)
				:accessor port-bucket)
   (name :initform "cl-translator"
		 :accessor name)
   (version :initform 1
			:accessor version)))

(defun create-translator (&optional (base-class 'translator))
  (let ((translator (make-instance base-class)))
    (with-accessors ((id-port identity-port)) translator
      ; destroy identity port when translator goes away
      (finalize translator (lambda () (port-destroy id-port))))
	;; some extra error checking goes here
	translator))

(defmethod setup ((trans translator) &optional (flags nil))
  (with-port (bootstrap (get-bootstrap-port))
	(with-accessors ((bucket port-bucket)
					 (control control-class)) trans
	  (with-port-info (port bucket control)
		(with-port (send-right (get-send-right port))
		  (fsys-startup bootstrap flags send-right :copy-send))))))

(defmethod run ((trans translator))
  (with-accessors ((bucket port-bucket)) trans
    (ports-manage-operations-one-thread bucket
					#'translator-demuxer)))

(defparameter *translator* (create-translator))

(def-fs-interface :file-statfs ((file :int) (buf :pointer))
		  (print "statfs")
		  :operation-not-supported)


(if (numberp (setup *translator*))
  (run *translator*))

;; example:
;; (defclass zip-translator ()
;;   ((name :initform "zip-translator")
;;    (version :initform "1")))

