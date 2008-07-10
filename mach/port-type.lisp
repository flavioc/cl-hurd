
(in-package :mach)

;;
;; In this file we define the foreign type mach_port_type_t.
;;

(defun %mach-port-type-get (code)
  "Does the same as MACH_PORT_TYPE(right) from mach/port.h"
  (ash 1 (+ code 16)))

;;
;; The bits for each type follow...
;;

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

;;
;; Group everything in a list for easy manipulation
;; in type translation.
;;
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

(define-foreign-type port-type-single-type ()
  ()
  (:documentation "CFFI type for simple port types.")
  (:actual-type :unsigned-int)
  (:simple-parser port-type-single-t))

(defun translate-type-bits (value)
  (translate-foreign-list value +port-type-codes+ 'to))

(defmethod translate-from-foreign (value (type port-type-single-type))
  "Translate a foreign value into a symbol."
  (translate-foreign-list value +port-type-codes+ 'from))

(defmethod translate-to-foreign (value (type port-type-single-type))
  "Translate a symbol into a foreign value."
  (translate-type-bits value))

(defclass port-type-class ()
  ((value :initarg :value
          :accessor value))
  (:documentation "Port type object for port types."))

(defmethod port-type-is-p ((ptype port-type-class) flag)
  "Checks if the port type object as a flag activated in its bitfield."
  (let ((val (value ptype))
        (bits (translate-type-bits flag)))
    (when bits
      (eq bits (boole boole-and val bits)))))

(define-foreign-type port-type-type ()
  ()
  (:documentation "CFFI type for port types.")
  (:actual-type :unsigned-int)
  (:simple-parser port-type-t))

(defmethod translate-from-foreign (value (type port-type-type))
  "Translate a foreign bitfield into a port type object."
  (make-instance 'port-type-class :value value))

(defmethod translate-to-foreign (val (type port-type-type))
  "Translate a port type object into a foreign bitfield."
  (value val))
