
(in-package :hurd)

;;
;; This file implements translator boxes
;; They tells if there is a translator set in a node.
;; 

(defclass transbox ()
  ((active :initarg :active
           :accessor active
           :initform nil
           :documentation "Is the box active?")
   (node :initform nil
         :initarg :node
         :accessor node
         :documentation "Node where it is set"))
  (:documentation "The transbox class"))

(defun make-transbox (node)
  "Creates a new transbox object on node."
  (let ((obj (make-instance 'transbox :node node)))
    ; Deallocate port when the object goes away
    (finalize obj (lambda ()
                    (when (active obj)
                      (port-deallocate (active obj)))))
    obj))

(defmethod box-translated-p ((box transbox))
  "Is there an active translator on this box?"
  (with-accessors ((port active)) box
    (port-valid port)))

(defmethod box-fetch-control ((box transbox))
  "Fetch a new control port from a translator box."
  (assert (box-translated-p box))
  (port-mod-refs (active box) 1)
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
    ; See if the active name is dead
    (if (zerop (port-get-refs (active box) :right-dead-name))
      (return-from box-set-active nil)))
  (when (active box)
    (port-deallocate (active box)))
  (setf (active box) port)
  t)
