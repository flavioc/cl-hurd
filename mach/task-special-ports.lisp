
(in-package :mach)

;;
;; In this file we implement the special port type
;; that can be passed to task_get_special_port.
;;

(defcenum special-port-type
  (:task-kernel-port 1)
  (:task-exception-port 3)
  (:task-bootstrap-port 4))

