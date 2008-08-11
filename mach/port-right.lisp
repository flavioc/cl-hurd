
(in-package :mach)

;;
;; The port right type. Represents different kinds of port rights.
;; 

(defcenum port-right
  (:right-send 0)
  :right-receive
  :right-send-once
  :right-port-set
  :right-dead-name
  :right-number)

