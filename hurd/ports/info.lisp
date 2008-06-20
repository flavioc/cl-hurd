
(defcstruct port-info
			(port-class :pointer)
			(refcnt :int)
			(weakrefcnt :int)
			(mscount port-mscount)
			(cancel-threshold msg-seqno)
			(flags :int)
			(port-right port)
			(rpc-info :pointer)
			(port-bucket :pointer)
			(hentry :pointer)
			(next :pointer)
			(prev :pointer))

(assert (= (foreign-type-size 'port-info) 48))

(defclass port-info ()
  ((pointer :initform (error "Please provide a pointer")
			:initarg :pointer
			:accessor pointer)))

(defcfun ("ports_port_deref" %ports-port-deref)
		 :void
		 (port :pointer))

(defun create-port-info (pointer)
  (let ((info (make-instance 'port-info :pointer pointer)))
	(with-accessors ((ptr pointer)) info
	  (finalize info (lambda () (%ports-port-deref ptr))))))

(defmethod translate-from-foreign (value (type port-info-type))
  (unless (null-pointer-p value)
	(create-port-info value)))

(defmethod translate-to-foreign (value (type port-info-type))
  (cond
	((null value) (null-pointer))
	(t
	  (pointer value))))

(defcfun ("ports_get_send_right" %ports-get-send-right)
		 port
		 (port :pointer))

(defmethod get-send-right ((port port-info))
  (%ports-get-send-right (pointer port)))
