
(in-package :hurd-translator)

;;
;; This file implements the node class.
;; A node represents a file in the translator's file system.
;;

(defclass node ()
   ((owner :initform 0
           :initarg :owner
           :accessor owner
           :documentation "The node's owner.")
    (stat :initform (make-stat)
          :initarg :stat
          :accessor stat
          :documentation "Stat information about the node.")
    (references :initform 1
                :accessor references
                :documentation "Number of references to this node.")
    (box :initform nil
         :accessor box
         :documentation "Node's translator box."))
   (:documentation "The node class."))

(defmethod inc-refs ((node node) &optional (cnt 1))
  "Increments 'node' references."
  (warn "inc-refs ~s" node)
  (incf (references node) cnt))

(defmethod dec-refs ((node node) &optional (cnt 1))
  "Decrements 'node' references."
  (warn "dec-refs ~s" node)
  (decf (references node) cnt))

(defmethod no-refs-p ((node node))
  "Tells if node has no references."
  (zerop (references node)))

(defmethod pre-drop-node ((node node))
  "Does some operations before we can drop a node."
  (transbox-drop (box node)))

(defmethod initialize-instance :after ((node node) &key)
  "Set the node's transbox (note that we need the node reference to do that)."
  (setf (box node) (make-transbox node)))

(defmethod print-object ((node node) stream)
  "Print a node to stream."
  (format stream "#<node ref: ~s>" (references node)))

