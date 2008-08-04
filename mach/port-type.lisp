
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

(defmacro %create-port-type-type ()
  `(defbitfield port-type-t
    (:type-none 0)
    (:type-send ,+type-send+)
    (:type-receive ,+type-receive+)
    (:type-send-once ,+type-send-once+)
    (:type-port-set ,+type-port-set+)
    (:type-dead-name ,+type-dead-name+)
    (:type-send-receive ,+type-send-receive+)
    (:type-send-rights ,+type-send-rights+)
    (:type-port-rights ,+type-port-rights+)
    (:type-port-or-dead ,+type-port-or-dead+)
    (:type-all-rights ,+type-all-rights+)
    (:type-dnrequest ,+type-dnrequest+)
    (:type-compat ,+type-compat+)))

(%create-port-type-type)

