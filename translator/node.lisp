
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
    (box :initform nil
         :accessor box
         :documentation "Node's translator box.")
    (link :initform nil
          :documentation "If this is symlink, this is the target file."))
   (:documentation "The node class."))

(defmethod pre-drop-node ((node node))
  "Does some operations before we can drop a node."
  (box-drop (box node)))

(defgeneric initialize-node (node))

(defmethod initialize-instance :after ((node node) &key)
  "Set the node's transbox (note that we need the node reference to do that)."
  (setf (box node) (make-instance 'node-transbox :node node))
  (initialize-node node))

(defmethod print-object ((node node) stream)
  "Print a node to stream."
  (format stream "#<node owner=~s link=~s box="
          (owner node)
          (link node))
  (print-object (box node) stream)
  (format stream ">"))

(defmethod is-controller-p ((node node) (user iouser))
  "Specialize is-controller-p for nodes."
  (is-controller-p (stat node) user))

(defmethod is-owner-p ((node node) (user iouser))
  "Specialize is-owner-p for nodes."
  (is-owner-p (stat node) user))

(defmethod has-access-p ((node node) (user iouser) flag)
  "Specialize has-access-p for nodes."
  (has-access-p (stat node) user flag))

(defmethod can-modify-dir-p ((node node) (user iouser))
  "Specialize can-modify-dir-p."
  (can-modify-dir-p (stat node) user))

(defmethod set-link-node ((node node) new-link)
  "When defining a new link target, change stat size."
  (setf (stat-get (stat node) 'size)
        (if (null new-link) 0 (length new-link)))
  (when new-link
    (set-type (stat node) :lnk))
  (setf (slot-value node 'link) new-link))

(defsetf link set-link-node)

(defmethod link ((node node))
  (cond
    ((is-lnk-p (stat node)) (slot-value node 'link))
    (t nil)))
