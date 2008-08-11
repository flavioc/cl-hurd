
(in-package :hurd-translator)

;;
;; This file implements protid's.
;; They group a user and a open node.
;;

(defclass protid (port-info)
  ((user :initform nil
         :initarg :user
         :accessor user
         :documentation "The user that opened the node.")
   (open-node :initform nil
              :initarg :open-node
              :accessor open-node
              :documentation "The open node."))
  (:documentation "The protid class."))

(defun make-protid (user open-node)
  "Create a new protid."
  (make-instance 'protid
                 :user user
                 :open-node open-node))

(defmethod get-stat ((protid protid))
  "Get stat information about the node opened."
  (stat (get-node protid)))

(defmethod get-box ((protid protid))
  "Get the transbox from the node."
  (box (get-node protid)))

(defmethod get-node ((protid protid))
  "Get the opened node."
  (refers (open-node protid)))

(defmethod get-user ((protid protid))
  "Get the user."
  (slot-value protid 'user))

(defmethod get-shadow-root ((protid protid))
  "Get the shadow root."
  (shadow-root (open-node protid)))

(defmethod get-shadow-root-parent ((protid protid))
  "Get the shadow root parent."
  (shadow-root-parent (open-node protid)))

(defmethod get-root-parent ((protid protid))
  "Get the root parent."
  (root-parent (open-node protid)))

(defmethod get-open-flags ((protid protid))
  "Get the open flags."
  (flags (open-node protid)))

(defmethod initialize-node ((node node))
  (tg:finalize node
               (lambda ()
                 (pre-drop-node node)
                 (when *translator*
                   (drop-node *translator* node)))))

(defmethod initialize-instance :after ((protid protid) &key)
  "Increment number of user nodes. If current count is 0, report it."
  (let ((node (get-node protid)))
    (when (zerop (num-users node))
      (report-new-user *translator* node))
    (inc-users node)))

(defmethod port-cleanup :after ((protid protid))
  "When cleaning up this port, decrease number of users. Report it if it drops to 0."
  (let ((node (get-node protid)))
    (dec-users node)
    (when (zerop (num-users node))
      (report-no-users *translator* node))))

