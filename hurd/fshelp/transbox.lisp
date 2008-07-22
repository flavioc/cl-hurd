
(in-package :hurd)

;;
;; This file implements translator boxes
;; They tells if there is a translator set in a node.
;; 

(defclass transbox ()
  ((active :initarg :active
           :accessor active
           :initform nil
           :documentation "Is the box active?"))
  (:documentation "The transbox class"))

(defmethod initialize-instance :after ((transbox transbox) &key)
  (warn "setting finalize")
  (tg:finalize transbox (lambda ()
                          (when (active transbox)
                            (port-deallocate (active transbox)))))
  transbox)

(defmethod box-translated-p ((box transbox))
  "Is there an active translator on this box?"
  (with-accessors ((port active)) box
    (port-valid-p port)))

(defmethod box-fetch-control ((box transbox))
  "Fetch a new control port from a translator box."
  (assert (box-translated-p box))
  (warn "box: active refs ~s" (port-get-refs (active box) :right-dead-name))
  (port-mod-refs (active box) :right-send 1)
  (active box))

(defmethod box-drop ((box transbox))
  "Drops a transbox."
  (when (active box)
    (port-deallocate (active box))
    (setf (active box) nil)))

(defmethod box-set-active ((box transbox) port excl-p)
  "Set a new active port on a box."
  (when (and excl-p
             (active box))
    (warn "box: excl and active! refs ~s" (port-get-refs (active box) :right-dead-name))
    ; See if the active name is dead
    (if (zerop (port-get-refs (active box) :right-dead-name))
      (return-from box-set-active nil)))
  (when (active box)
    (warn "box: deallocate old active")
    (port-deallocate (active box)))
  (warn "box: setting new ~s" port)
  (setf (active box) port)
  t)
