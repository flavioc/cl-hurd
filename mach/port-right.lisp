
(in-package :mach)

(define-foreign-type port-right-type ()
  ()
  (:documentation "CFFI type for mach_port_right_t")
  (:actual-type :unsigned-int)
  (:simple-parser port-right))

;; All the possible port right values.
;; Found at mach/port.h.
(defconstant +right-send+ 0)
(defconstant +right-receive+ 1)
(defconstant +right-send-once+ 2)
(defconstant +right-port-set+ 3)
(defconstant +right-dead-name+ 4)
(defconstant +right-number+ 5)

;; As usual, everything is grouped together into a list.
(defconstant +port-right-codes+
  `((,+right-send+ :right-send)
    (,+right-receive+ :right-receive)
    (,+right-send-once+ :right-send-once)
    (,+right-port-set+ :right-port-set)
    (,+right-dead-name+ :right-dead-name)
    (,+right-number+ :right-number)))

(defmethod translate-from-foreign (value (type port-right-type))
  "Translate a foreign value into a symbol."
  (translate-foreign-list value +port-right-codes+ :from))

(defmethod translate-to-foreign (value (type port-right-type))
  "Translate a symbol into a foreign value."
  (translate-foreign-list value +port-right-codes+ :to))

