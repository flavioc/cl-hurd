
(in-package :mach)

;;
;; This file defines the msg notify foreign codes.
;;

(defmacro %create-msg-notify ()
  (let* ((+notify-first+ #o100)
         (+notify-port-destroyed+ (+ +notify-first+ #o5))
         (+notify-dead-name+ (+ +notify-first+ #o10))
         (+notify-no-senders+ (+ +notify-first+ #o6)))
    `(defcenum msg-notify
       (:notify-port-destroyed ,+notify-port-destroyed+)
       (:notify-dead-name ,+notify-dead-name+)
       (:notify-no-senders ,+notify-no-senders+))))

(%create-msg-notify)

