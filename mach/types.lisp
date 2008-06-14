
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

(defmethod translate-from-foreign (value (type (eql 'port)))
  "Translates a port value to a more lispy one"
  (cond
	((eq +port-null+ value) nil)
	((eq +port-dead+ value) :dead)
	(t
	  value)))

(defmethod translate-to-foreign (value (type (eql 'port)))
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
