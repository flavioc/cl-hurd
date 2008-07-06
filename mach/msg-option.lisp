
(in-package :mach)

;;
;; This file defines the mach_msg_option_t foreign type.
;;

(defcenum msg-option
  (:msg-option-none 0)
  (:send-msg #x01)
  (:rcv-msg #x02)
  (:send-timeout #x010)
  (:send-notify #x020)
  (:send-interrupt #x040)
  (:send-cancel #x080)
  (:rcv-timeout #x0100)
  (:rcv-notify #x0200)
  (:rcv-interrupt #x0400)
  (:rcv-large #x0800))

