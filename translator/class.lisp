
(defun get-identity-port ()
  (let ((port (port-allocate :right-receive)))
    (if (not (port-valid port))
      (error "Invalid fsys identity port")
      port)))

(defclass translator ()
  ((identity-port :initform (get-identity-port)
		  :accessor identity-port)
   (port-bucket :initform (make-bucket)
		:accessor port-bucket)
   (underlying-node :initform nil)
   (name :initform "cl-translator"
	 :accessor name)
   (version :initform (list 1 0 0)
	    :accessor version)))

(defun create-translator (&optional (base-class 'translator))
  (let ((translator (make-instance base-class)))
    (with-accessors ((id-port identity-port)) translator
      ; destroy identity port when translator goes away
      (finalize translator (lambda () (port-destroy id-port))))
    translator))

(defmethod setup ((trans translator) &optional (flags nil))
  (with-port-deallocate (bootstrap (get-bootstrap-port))
    (let ((port (add-port (port-bucket trans))))
      (with-port-deallocate (right (get-send-right port))
        (let ((file (fsys-startup bootstrap flags right :copy-send)))
	  (warn "file ~a~%" file)
          (setf (slot-value trans 'underlying-node) file)
	  t)))))

(defmethod run ((trans translator))
  (let ((*translator* trans))
    (run-server (lambda (port in out)
		  (if (has-port (port-bucket trans) port)
		    (warn "port found~%")
		    (warn "port not found~%"))
		  (translator-demuxer in out))
		(port-bucket trans))))


(def-fs-interface :file-statfs ((file :int) (buf :pointer))
		  (print "statfs")
		  :operation-not-supported)

(defparameter *mytranslator* (create-translator))

(when (setup *mytranslator*)
  (run *mytranslator*))

;; example:
;; (defclass zip-translator ()
;;   ((name :initform "zip-translator")
;;    (version :initform "1")))

