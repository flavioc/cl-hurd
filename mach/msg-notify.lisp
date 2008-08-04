
(in-package :mach)

;;
;; This file defines the msg notify foreign codes.
;;

;; Possible codes follow...
(defconstant +notify-first+ #o100)
(defconstant +notify-port-destroyed+ (+ +notify-first+ #o5))
(defconstant +notify-dead-name+ (+ +notify-first+ #o10))
(defconstant +notify-no-senders+ (+ +notify-first+ #o6))

(defmacro %create-msg-notify ()
  `(defcenum msg-notify
    (:notify-port-destroyed ,+notify-port-destroyed+)
    (:notify-dead-name ,+notify-dead-name+)
    (:notify-no-senders ,+notify-no-senders+)))

(%create-msg-notify)

