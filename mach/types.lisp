
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

(defconstant +right-send+ 0)
(defconstant +right-receive+ 1)
(defconstant +right-send-once+ 2)
(defconstant +right-port-set+ 3)
(defconstant +right-dead-name+ 4)
(defconstant +right-number+ 5)

(defconstant +port-right-codes+
	     `((,+right-send+ :right-send)
	       (,+right-receive+ :right-receive)
	       (,+right-send-once+ :right-send-once)
	       (,+right-port-set+ :right-port-set)
	       (,+right-dead-name+ :right-dead-name)
	       (,+right-number+ :right-number)))

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

;; mach_msg_type_name_t

(define-foreign-type msg-type-name-type ()
  ()
  (:actual-type :unsigned-int)
  (:simple-parser msg-type-name))

(defconstant +msg-type-name-codes+
	     '((16 :move-receive)
	       (17 :move-send)
	       (18 :move-send-once)
	       (19 :copy-send)
	       (20 :make-send)
	       (21 :make-send-once)))

(defmethod translate-from-foreign (value (type msg-type-name-type))
  (translate-foreign-list value +msg-type-name-codes+ 'from))

(defmethod translate-to-foreign (value (type msg-type-name-type))
  (translate-foreign-list value +msg-type-name-codes+ 'to))

;; mach_port_delta_t
;; a boring integer

(defctype port-delta :int)

;; mach_port_type

(defun %mach-port-type-get (code)
  (ash 1 (+ code 16)))

(defconstant +type-send+ (%mach-port-type-get +right-send+))
(defconstant +type-receive+ (%mach-port-type-get +right-receive+))
(defconstant +type-send-once+ (%mach-port-type-get +right-send-once+))
(defconstant +type-port-set+ (%mach-port-type-get +right-port-set+))
(defconstant +type-dead-name+ (%mach-port-type-get +right-dead-name+))
(defconstant +type-send-receive+
	     (boole boole-ior
		    +type-send+
		    +type-receive+))
(defconstant +type-send-rights+
	     (boole boole-ior
		    +type-send+
		    +type-send-once+))
(defconstant +type-port-rights+
	     (boole boole-ior
		    +type-send-rights+
		    +type-receive+))
(defconstant +type-port-or-dead+
	     (boole boole-ior
		    +type-port-rights+
		    +type-dead-name+))
(defconstant +type-all-rights+
	     (boole boole-ior
		    +type-port-or-dead+
		    +type-port-set+))
(defconstant +type-dnrequest+ #x80000000)
(defconstant +type-marequest+ #x40000000)
(defconstant +type-compat+ #x20000000)

(defconstant +port-type-codes+
	     `((0 :type-none)
	       (,+type-send+ :type-send)
	       (,+type-receive+ :type-receive)
	       (,+type-send-once+ :type-send-once)
	       (,+type-port-set+ :type-port-set)
	       (,+type-dead-name+ :type-dead-name)
	       (,+type-send-receive+ :type-send-receive)
	       (,+type-send-rights+ :type-send-rights)
	       (,+type-port-rights+ :type-port-rights)
	       (,+type-port-or-dead+ :type-port-or-dead)
	       (,+type-all-rights+ :type-all-rights)
	       (,+type-dnrequest+ :type-dnrequest)
	       (,+type-compat+ :type-compat)))

(defclass port-type-class ()
  ((value :initarg :value
	  :accessor value)))

(defmethod is ((ptype port-type-class) flag)
  (let ((val (value ptype))
	(bits (find flag +port-type-codes+ :key #'second)))
    (when bits
      (not (zerop (boole boole-and val (first bits)))))))

(define-foreign-type port-type-type ()
  ()
  (:actual-type :unsigned-int)
  (:simple-parser port-type-t))

(defmethod translate-from-foreign (value (type port-type-type))
  (make-instance 'port-type-class :value value))

(defmethod translate-to-foreign (val (type port-type-type))
  (value val))

;; mach_msg_id_t

(defconstant +notify-first+ #o100)
(defconstant +notify-port-destroyed+ (+ +notify-first+ #o5))
(defconstant +notify-dead-name+ (+ +notify-first+ #o10))
(defconstant +notify-no-senders+ (+ +notify-first+ #o6))

(defconstant +msg-id-codes+
	     `((,+notify-port-destroyed+ :notify-port-destroyed)
	       (,+notify-dead-name+ :notify-dead-name)
	       (,+notify-no-senders+ :notify-no-senders)))

(define-foreign-type msg-id-type ()
  ()
  (:actual-type :int)
  (:simple-parser msg-id))

(defmethod translate-from-foreign (value (type msg-id-type))
  (translate-foreign-list value +msg-id-codes+ 'from))

(defmethod translate-to-foreign (value (type msg-id-type))
  (translate-foreign-list value +msg-id-codes+ 'to))
