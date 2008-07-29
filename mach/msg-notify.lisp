
(in-package :mach)

;;
;; This file defines the msg notify foreign codes.
;;

;; Possible codes follow...
(defconstant +notify-first+ #o100)
(defconstant +notify-port-destroyed+ (+ +notify-first+ #o5))
(defconstant +notify-dead-name+ (+ +notify-first+ #o10))
(defconstant +notify-no-senders+ (+ +notify-first+ #o6))

;; Everything into a single list...
(defconstant +msg-notify-codes+
  `((,+notify-port-destroyed+ :notify-port-destroyed)
    (,+notify-dead-name+ :notify-dead-name)
    (,+notify-no-senders+ :notify-no-senders)))

(define-foreign-type msg-notify-type ()
  ()
  (:documentation "CFFI type for notification codes.")
  (:actual-type :int)
  (:simple-parser msg-notify))

(defmethod translate-from-foreign (value (type msg-notify-type))
  "Translate a foreign value into a symbol."
  (translate-foreign-list value +msg-notify-codes+ :from))

(defmethod translate-to-foreign (value (type msg-notify-type))
  "Translate a symbol into a foreign value."
  (translate-foreign-list value +msg-notify-codes+ :to))

