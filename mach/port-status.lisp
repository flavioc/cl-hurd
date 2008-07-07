
(in-package :mach)

(use-package :trivial-garbage)

;; This structure contains some status information about a port, which can be queried with port-get-receive-status.
;;
(defcstruct port-status-struct
  (port-set port)
  (seqno port-seqno)
  (mscount port-mscount)
  (queue-limit port-msgcount)
  (msgcount port-msgcount)
  (so-rights port-rights)
  (has-send-rights :boolean)
  (port-deleted-notification-requested :boolean)
  (no-senders-notification-requested :boolean))

(defclass port-status ()
  ((ptr :initarg :ptr
        :accessor ptr
        :documentation "Pointer to a port-status structure."))
  (:documentation "Class for port-status objects containing port information."))

(defmethod port-status-get ((status port-status) what)
  "Get a specific field from a port-status."
  (foreign-slot-value (ptr status) 'port-status-struct what))

(defmethod port-status-has-send-rights-p ((status port-status))
  "Return T if the port-status has send rights."
  (port-status-get status 'has-send-rights))

(defmethod port-status-has-port-deleted-notification-p ((status port-status))
  "Return T if the port from port-status has requested a port-deleted notification."
  (port-status-get status 'port-deleted-notification-requested))

(defmethod port-status-has-no-senders-notification-p ((status port-status))
  "Return T if the port from port-status has requested a no-senders notification."
  (port-status-get status 'no-senders-notification-requested))

(defun make-port-status (ptr)
  "Creates a new port-status using ptr as the status structure."
  (let ((mem (foreign-alloc 'port-status-struct)))
    (memcpy mem ptr (foreign-type-size 'port-status-struct))
    (let ((obj (make-instance 'port-status :ptr mem)))
      (finalize obj (lambda ()
                      (foreign-free mem)))
      obj)))

