
(in-package :mach)

;;
;; This file implements the foreign type mach_msg_type_name_t.
;;

(define-foreign-type msg-type-name-type ()
  ()
  (:documentation "CFFI type for msg-type-name's.")
  (:actual-type :unsigned-int)
  (:simple-parser msg-type-name))

;; As usual, we group the possible values into a list
;; with the respective lisp value.
(defconstant +msg-type-name-codes+
  '((16 :move-receive)
    (17 :move-send)
    (18 :move-send-once)
    (19 :copy-send)
    (20 :make-send)
    (21 :make-send-once)))

(defun translate-msg-type-name-foreign (value)
  (translate-foreign-list value +msg-type-name-codes+ :from))

(defmethod translate-from-foreign (value (type msg-type-name-type))
  "Translate a foreign message type name into a lisp symbol."
  (translate-msg-type-name-foreign value))

(defun translate-msg-type-name-symbol (value)
  (translate-foreign-list value +msg-type-name-codes+ :to))

(defmethod translate-to-foreign (value (type msg-type-name-type))
  "Translate a msg-type-name symbol into a foreign value."
  (translate-msg-type-name-symbol value))

