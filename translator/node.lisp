
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
    (linktarget :initform nil
                :accessor link
                :documentation "If this is symlink, this is the target file."))
   (:documentation "The node class."))

(defmethod pre-drop-node ((node node))
  "Does some operations before we can drop a node."
  (box-drop (box node)))

(defgeneric initialize-node (node))

(defmethod initialize-instance :after ((node node) &key)
  "Set the node's transbox (note that we need the node reference to do that)."
  (setf (box node) (make-transbox node))
  (initialize-node node))

(defmethod print-object ((node node) stream)
  "Print a node to stream."
  (format stream "#<node owner=~s>" (owner node)))

(defmethod is-controller-p ((node node) (user iouser))
  "Specialize is-controller-p for nodes."
  (is-controller-p (stat node) user))

(defmethod is-owner-p ((node node) (user iouser))
  "Specialize is-owner-p for nodes."
  (is-owner-p (stat node) user))

(defmethod has-access-p ((node node) (user iouser) flag)
  "Specialize has-access-p for nodes."
  (has-access-p (stat node) user flag))
