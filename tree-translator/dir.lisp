
(in-package :hurd-tree-translator)

;;
;; This file implements the special directory node and entry node.
;;

(defvar *ino-value* 1)

(defclass entry (node)
  ((name :initform nil
         :initarg :name
         :accessor name
         :documentation "The file name.")
   (parent :initform nil
           :initarg :parent
           :accessor parent
           :documentation "Parent node."))
  (:documentation "An entry, with a name and a parent."))

(defmethod print-object ((node entry) stream)
  "Print an entry to stream."
  (format stream "#<entry name: ~s node: " (name node))
  (call-next-method)
  (format stream ">"))

(defclass dir-entry (entry)
  ((entries :initform (make-hash-table :test 'equal)
            :accessor entries
            :documentation "The directory entries."))
  (:documentation "A special entry: the directory entry."))

(defun %new-ino-val (stat)
  "Sets and increments the ino value of a stat struct."
  (setf (stat-get stat 'ino) (incf *ino-value*)))

(defun make-dir (name stat &optional (parent nil))
  "Creates a new directory node."
  (set-type stat 'dir)
  (%new-ino-val stat)
  (setf (stat-get stat 'nlink) 1)
  (make-instance 'dir-entry :stat stat :name name :parent parent))

(defmethod add-entry ((dir dir-entry) (entry entry))
  "Adds a new entry to the directory node 'dir'."
  (setf (gethash (name entry) (entries dir)) entry))

(defmethod setup-entry ((entry entry))
  "Changes some node information to sane defaults."
  (set-type (stat entry) 'reg)
  (%new-ino-val (stat entry))
  (setf (stat-get (stat entry) 'nlink) 1))

(defun make-entry (name stat &optional (parent nil))
  "Creates a new entry."
  (let ((obj (make-instance 'entry :stat stat :name name :parent parent)))
    (setup-entry obj)
    obj))

(defmethod dir-size ((dir dir-entry))
  "Returns number of entries in a directory."
  (hash-table-count (entries dir)))

(defmethod get-entry ((dir dir-entry) (entry string))
  "Gets an entry from a directory based on the filename 'entry'."
  (gethash entry (entries dir)))

(defmethod remove-dir-entry ((dir dir-entry) (entry string))
  "Removes a directory entry with name 'entry'."
  (remhash entry (entries dir)))
