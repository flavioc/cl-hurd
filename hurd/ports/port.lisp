
(in-package :hurd)

;; 
;; This file implements a port abstraction like port_info from libports.
;; A port-info has send count and a receive right, we can also get new rights from it.
;;

(defclass port-info ()
  ((right :initform (port-allocate :right-receive)
          :accessor port-right
          :documentation "Right for this port")
   (mscount :initform 0
            :accessor mscount
            :documentation "Send rights count")))

(defmethod deallocate-send-right ((port port-info))
  "Deallocates the port's send right."
  (port-deallocate (port-right port)))

(defmethod get-right ((port port-info))
  "Gives the port right, incrementing the send count and requesting a notification when it goes away."
  (when (zerop (mscount port))
    (port-request-notification (port-right port)
                               :notify-no-senders
                               (1+ (mscount port))
                               (port-right port)
                               :make-send-once))
  (incf (mscount port))
  (port-right port))

(defmethod get-send-right ((port port-info))
  "Gets a send right for 'port'."
  (let ((right (get-right port)))
    (port-insert-right right right :make-send)
    right))

(defmethod port-cleanup ((port port-info))
  "Cleanup routine for ports."
  (when (port-right port)
    (setf (mscount port) 0)
    (port-mod-refs (port-right port) :right-receive -1)
    (setf (port-right port) nil))
  t)
