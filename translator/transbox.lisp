
(in-package :hurd-translator)

(defclass node-transbox (transbox)
  ((node :initform nil
         :initarg :node
         :accessor node
         :documentation "Node this refers to."))
  (:documentation "Transbox used in nodes."))

(defmethod box-set-active ((box node-transbox) port excl-p)
  (declare (ignore excl-p))
  (let ((ret (call-next-method)))
    (when ret
      ; Update active-translator node stat field to most updated information.
      (set-active-trans (stat (node box))
                        (port-valid-p port)))
    ret))
