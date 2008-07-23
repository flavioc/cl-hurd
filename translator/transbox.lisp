
(in-package :hurd-translator)

(defclass node-transbox (transbox)
  ((node :initform nil
         :initarg :node
         :accessor node
         :documentation "Node this refers to."))
  (:documentation "Transbox used in nodes."))

(defmethod box-set-active ((box node-transbox) port excl-p)
  "Update the active stat bit when we change the transbos port."
  (declare (ignore excl-p))
  (let ((ret (call-next-method)))
    (when ret
      ; Update active-translator node stat field to most updated information.
      (set-active-trans (stat (node box))
                        (port-valid-p port)))
    ret))

(defmethod box-set-passive ((box node-transbox) path)
  "Update the passive stat bit when we change the transbox path."
  (let ((ret (call-next-method)))
    (when ret
      (set-passive-trans (stat (node box))
                         (if (null path) nil t)))
    ret))

(defmethod passive ((box node-transbox))
  "Only return the passive path when the stat bit is sane."
  (cond
    ((has-passive-trans-p (stat (node box)))
     (call-next-method))
    (t nil)))

(defmethod box-passive-p ((box node-transbox))
  "To be passive the stat bit must be set."
  (and (call-next-method)
       (has-passive-trans-p (stat (node box)))))
