
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
  (case value
	((+port-null+) nil)
	((+port-dead+) :dead)
	(t
	  value)))

(defmethod translate-to-foreign (value (type port-type))
  "Translates a lispy port value to a foreign one"
  (case value
	((nil) +port-null+)
	((:dead) +port-dead+)
	(t
	  value)))

;; task_t
(defctype task port)

;; ipc_space_t
(defctype ipc-space task)
