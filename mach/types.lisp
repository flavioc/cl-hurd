
; mach_port_t
(define-foreign-type port-type ()
  ()
  (:actual-type :unsigned-int)
  (:simple-parser port))

;;; error return codes for ports

; MACH_PORT_NULL is 0
(defconstant +port-null+ 0)

; MACH_PORT_DEAD is ~0 casted to mach_port_t
; which means that it's largest representable number in 'port' bits
(defconstant +port-dead+ (largest-representable-number
						   (num-bits (foreign-type-size 'port))))

(defmethod translate-from-foreign (value (type port-type))
  "Translates a port value to a more lispy one"
  (cond
	((eq +port-null+ value) nil)
	((eq +port-dead+ value) :dead)
	(t
	  value)))

(defmethod translate-to-foreign (value (type port-type))
  "Translates a lispy port value to a foreign one"
  (cond
	((null value) +port-null+)
	((eq value :dead) +port-null)
	(t
	  value)))

;; task_t
(defctype task port)

;; ipc_space_t
(defctype ipc-space task)

;; mach_port_t*: a pointer to a port
(defctype port-pointer :pointer)

;; mach_port_right_t (natural_t): port rights
(define-foreign-type port-right-type ()
  ()
  (:actual-type :unsigned-int)
  (:simple-parser port-right))

(defconstant +port-right-codes+
	     '((0 :right-send)
	       (1 :right-receive)
	       (2 :right-send-once)
	       (3 :right-port-set)
	       (4 :right-dead-name)
	       (5 :right-number)))

(defmethod translate-from-foreign (value (type port-right-type))
  (translate-foreign-list value +port-right-codes+ 'from))

(defmethod translate-to-foreign (value (type port-right-type))
  (translate-foreign-list value +port-right-codes+ 'to))

;; mach/task_special_ports.h
;; special-port-type: the which argument to task_get_special_port.

(define-foreign-type port-special-port-type ()
  ()
  (:actual-type :int)
  (:simple-parser special-port-type))

(defconstant +special-port-type-codes+
	     '((1 :task-kernel-port)
	       (3 :task-exception-port)
	       (4 :task-bootstrap-port)))

(defmethod translate-from-foreign (value (type port-special-port-type))
  (translate-foreign-list value +special-port-type-codes+ 'from))

(defmethod translate-to-foreign (value (type port-special-port-type))
  (translate-foreign-list value +special-port-type-codes+ 'to))

;; mach/port.h
;; mach_port_msgcount_t

(defctype port-mscount :unsigned-int)

;; mach_msg_seqno_t

(defctype msg-seqno :unsigned-int)
