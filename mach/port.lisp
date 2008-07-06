
(in-package :mach)

;;
;; This file defines the mach_port_t foreign type.
;;

(define-foreign-type port-type ()
  ()
  (:documentation "mach_port_t CFFI type.")
  (:actual-type :unsigned-int)
  (:simple-parser port))

(defconstant +port-null+ 0 "MACH_PORT_NULL")

;; MACH_PORT_DEAD is ~0 casted to mach_port_t
;; which means that it's largest representable number in 'port' bits
(defconstant +port-dead+ (largest-representable-number
                           (num-bits (foreign-type-size 'port))))

(defmethod translate-from-foreign (value (type port-type))
  "Translates a port value to a more lispy one"
  (cond
    ((eq +port-null+ value) nil) ; MACH_PORT_NULL is just... nil.
    ((eq +port-dead+ value) :dead) ; MACH_PORT_DEAD is :dead.
    (t ; Else, it's a 'boring' integer.
      value)))

(defmethod translate-to-foreign (value (type port-type))
  "Translates a lispy port value to a foreign one"
  (cond
    ((null value) +port-null+)
    ((eq value :dead) +port-null+)
    (t
      value)))

