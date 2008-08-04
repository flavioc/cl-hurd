
(in-package :mach)

;;
;; This file implements the foreign type mach_msg_type_name_t.
;;

(defcenum msg-type-name
  (:move-receive 16)
  (:move-send 17)
  (:move-send-once 18)
  (:copy-send 19)
  (:make-send 20)
  (:make-send-once 21))

(defun translate-msg-type-name-symbol (sym)
  (foreign-enum-value 'msg-type-name sym))

