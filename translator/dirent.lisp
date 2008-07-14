
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
   (ino :initarg :ino
        :accessor ino
        :documentation "Ino value of this entry.")
   (filetype :initarg :file-type
             :accessor file-type
             :documentation "Mode type of this entry."))
  (:documentation "Dirent class with name + node."))

(defun make-dirent (name ino file-type)
  "Creates a new dirent object."
  (make-instance 'dirent :name name :ino ino :file-type file-type))

(defun make-node-dirent (name node)
  "Creates a new dirent object based on a standard node."
  (make-dirent name
               (stat-get (stat node) 'ino)
               (stat-get (stat node) 'type)))

(defmethod print-object ((dirent dirent) stream)
  (format stream "#<dirent name=~s ino=~s filetype=~s>"
          (name dirent)
          (ino dirent)
          (file-type dirent)))

