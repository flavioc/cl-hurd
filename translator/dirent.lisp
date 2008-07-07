
(in-package :hurd-translator)

;;
;; This file implements the dirent class.
;; It serves as a way to translators tell
;; what entries they have in a directory.
;;

(defclass dirent ()
  ((name :initarg :name
         :accessor name
         :documentation "Entry name")
   (node :initarg :node
         :accessor node
         :documentation "Node this entry refers to."))
  (:documentation "Dirent class with name + node."))

(defun make-dirent (name node)
  "Creates a new dirent object."
  (make-instance 'dirent :name name :node node))
