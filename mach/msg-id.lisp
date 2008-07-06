
(in-package :mach)

;;
;; This file defines the mach_msg_id_t foreign type.
;;

;; Possible codes follow...
(defconstant +notify-first+ #o100)
(defconstant +notify-port-destroyed+ (+ +notify-first+ #o5))
(defconstant +notify-dead-name+ (+ +notify-first+ #o10))
(defconstant +notify-no-senders+ (+ +notify-first+ #o6))

;; Everything into a single list...
(defconstant +msg-id-codes+
  `((,+notify-port-destroyed+ :notify-port-destroyed)
    (,+notify-dead-name+ :notify-dead-name)
    (,+notify-no-senders+ :notify-no-senders)))

(define-foreign-type msg-id-type ()
  ()
  (:documentation "CFFI type for mach_msg_id_t.")
  (:actual-type :int)
  (:simple-parser msg-id))

(defmethod translate-from-foreign (value (type msg-id-type))
  "Translate a foreign value into a symbol."
  (translate-foreign-list value +msg-id-codes+ 'from))

(defmethod translate-to-foreign (value (type msg-id-type))
  "Translate a symbol into a foreign value."
  (translate-foreign-list value +msg-id-codes+ 'to))

