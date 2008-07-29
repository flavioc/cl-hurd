
(in-package :mach)

;;
;; In this file we implement the special port type
;; that can be passed to task_get_special_port.
;;

(define-foreign-type port-special-port-type ()
  ()
  (:documentation "CFFI type for special port types.")
  (:actual-type :int)
  (:simple-parser special-port-type))

;; All possible codes and symbols in a list.
(defconstant +special-port-type-codes+
  '((1 :task-kernel-port)
    (3 :task-exception-port)
    (4 :task-bootstrap-port)))

(defmethod translate-from-foreign (value (type port-special-port-type))
  "Translate a foreign code into a symbol."
  (translate-foreign-list value +special-port-type-codes+ :from))

(defmethod translate-to-foreign (value (type port-special-port-type))
  "Translate a symbol into a foreign value."
  (translate-foreign-list value +special-port-type-codes+ :to))

