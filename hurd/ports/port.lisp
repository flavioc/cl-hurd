
(in-package :hurd)

;; 
;; This file implements a port abstraction like port_info from libports.
;; A port-info has send count and a receive right, we can also get new rights from it.
;;

(defclass port-info ()
  ((right :initform (port-allocate :right-receive)
          :accessor port-right
          :documentation "Right for this port")
   (has-send-rights :initform nil
                    :documentation "If we have given send rights to someone")
   (mscount :initform 0
            :accessor mscount
            :documentation "Send rights count")
   (is-control :initform nil
               :initarg :control)))

(defmethod has-send-rights ((port port-info))
  "Has this port name send rights?"
  (with-slots ((has has-send-rights)) port
    has))

(defmethod set-send-rights ((port port-info) value)
  "Sets the has-send-rights field to 'value'."
  (declare (type boolean value))
  (setf (slot-value port 'has-send-rights) value))

(defmethod get-right ((port port-info))
  "Gives the port right, incrementing the send count and requesting a notification when it goes away."
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
  "Gets a send right for 'port'."
  (let ((right (get-right port)))
    (port-insert-right right right :make-send)
    right))

(define-condition port-still-has-send-righs (error)
  ((port :initarg port :reader port))
  (:documentation "Error delivered when we try to cleanup a port with send rights"))

(defmethod port-cleanup ((port port-info))
  "Cleanup routine for ports."
  (if (has-send-rights port)
    (error 'port-still-has-send-righs :port port))
  (port-destroy (port-right port)))

(defmethod port-is-control-p ((port port-info))
  "Tells if this port is a control port."
  (slot-value port 'is-control))

(defmethod port-is-user-p ((port port-info))
  "Tells if this port is an user port."
  (not (port-is-control-p port)))
