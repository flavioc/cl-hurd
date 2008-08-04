
(in-package :mach)

(defconstant +right-send+ 0)
(defconstant +right-receive+ 1)
(defconstant +right-send-once+ 2)
(defconstant +right-port-set+ 3)
(defconstant +right-dead-name+ 4)
(defconstant +right-number+ 5)

(defmacro %create-port-right-type ()
  `(defcenum port-right
    (:right-send ,+right-send+)
    (:right-receive ,+right-receive+)
    (:right-send-once ,+right-send-once+)
    (:right-port-set ,+right-port-set+)
    (:right-dead-name ,+right-dead-name+)
    (:right-number ,+right-number+)))

(%create-port-right-type)

