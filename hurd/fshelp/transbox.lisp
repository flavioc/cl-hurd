
(in-package :hurd)

;;
;; This file implements translator boxes
;; They tells if there is a translator set in a node.
;; 

(defclass transbox ()
  ((active :initarg :active
           :reader active
           :initform nil
           :documentation "Is the box active?")
   (passive :initarg :active
            :reader passive
            :initform nil
            :documentation "Passive translator path."))
  (:documentation "The transbox class"))

(defmethod initialize-instance :after ((transbox transbox) &key)
  (tg:finalize transbox (lambda ()
                          (when (active transbox)
                            (port-deallocate (active transbox)))))
  transbox)

(defmethod box-active-p ((box transbox))
  "Is there an active translator on this box?"
  (port-valid-p (active box)))

(defmethod box-passive-p ((box transbox))
  "Is there an passive translator on this box?"
  (not (null (passive box))))

(defmethod box-translated-p ((box transbox))
  "Is there an active or passive translator on this box?"
  (or (box-active-p box)
      (box-passive-p box)))

(defmethod box-fetch-control ((box transbox))
  "Fetch a new control port from a translator box."
  (assert (box-active-p box))
  (warn "box: active refs ~s" (port-get-refs (active box) :right-dead-name))
  (port-mod-refs (active box) :right-send 1)
  (active box))

(defmethod box-drop ((box transbox))
  "Drops a transbox."
  (when (active box)
    (port-deallocate (active box))
    (box-set-active box nil t)))

(defmethod box-set-active ((box transbox) port excl-p)
  "Set a new active port on a box."
  (when (and excl-p
             (active box)
             (zerop (port-get-refs (active box) :right-dead-name)))
    ; See if the active name is dead
    (return-from box-set-active nil))
  (when (active box)
    (warn "box: deallocate old active")
    (port-deallocate (active box)))
  (warn "box: setting new ~s" port)
  (setf (slot-value box 'active) port)
  t)

(defmethod box-set-passive ((box transbox) path)
  (setf (slot-value box 'passive) path)
  t)

(defsetf passive box-set-passive)

(defmethod box-set-active-foo ((box transbox) port)
  (box-set-active box port t))

(defsetf active box-set-active-foo)

(defmethod print-object ((box transbox) stream)
  (format stream "#<transbox active=~s passive=~s>"
          (active box)
          (passive box)))
