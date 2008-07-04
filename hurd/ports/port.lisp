
(defclass port-info ()
  ((right :initform (port-allocate :right-receive)
	  :accessor port-right
	  :documentation "Right for this port")
   (has-send-rights :initform nil)
   (mscount :initform 0
			:accessor mscount)))

(defmethod has-send-rights ((port port-info))
  (with-slots ((has has-send-rights)) port
    has))

(defmethod set-send-rights ((port port-info) value)
  (setf (slot-value port 'has-send-rights) value))

(defmethod get-right ((port port-info))
  (incf (mscount port))
  (unless (has-send-rights port)
	(set-send-rights port t)
	(port-request-notification (port-right port)
							   :notify-no-senders
							   (mscount port)
							   (port-right port)
							   :make-send-once))
  (port-right port))

(defmethod get-send-right ((port port-info))
  (let ((right (get-right port)))
    (port-insert-right right right :make-send)
    right))

(define-condition port-still-has-send-righs (error)
  ((port :initarg port :reader port)))

(defmethod port-cleanup ((port port-info))
  (if (has-send-rights port)
    (error 'port-still-has-send-righs :port port))
  (port-destroy (port-right port)))
