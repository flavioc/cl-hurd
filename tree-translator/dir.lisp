
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
  ((entries :initform (make-sorted-container #'string< #'name)
            :accessor entries
            :documentation "The directory entries."))
  (:documentation "A special entry: the directory entry."))

(defun %new-ino-val (stat)
  "Sets and increments the ino value of a stat struct."
  (setf (stat-get stat 'ino) (incf *ino-value*)))

(defun make-dir (name stat &optional (parent nil))
  "Creates a new directory node."
  (let ((obj (make-instance 'dir-entry :stat stat
                            :name name
                            :parent parent)))
    (setup-entry obj)
    obj))

(defmethod add-entry ((dir dir-entry) (entry entry))
  "Adds a new entry to the directory node 'dir'."
  (incf (stat-get (stat dir) 'nlink)) ; New entry.
  (insert-element (entries dir) entry))

(defmethod setup-entry ((entry entry))
  "Changes some node information to sane defaults."
  (set-type (stat entry) :reg)
  (%new-ino-val (stat entry))
  (setf (stat-get (stat entry) 'nlink) 1))

(defmethod setup-entry ((entry dir-entry))
  "Changes some node information to sane defaults on directories."
  (set-type (stat entry) :dir)
  (%new-ino-val (stat entry))
  ; nlink represents number of objects in a directory
  (setf (stat-get (stat entry) 'nlink) 2))

(defun make-entry (name stat &optional (parent nil))
  "Creates a new entry."
  (let ((obj (make-instance 'entry :stat stat :name name :parent parent)))
    (setup-entry obj)
    obj))

(defmethod dir-size ((dir dir-entry))
  "Returns number of entries in a directory."
  (+ 2 ; Entries "." and "..".
     (count-elements (entries dir))))

(defmethod get-entry ((dir dir-entry) (entry string))
  "Gets an entry from a directory based on the filename 'entry'."
  (get-element (entries dir) entry))

(defmethod remove-dir-entry ((dir dir-entry) (entry string))
  "Removes a directory entry with name 'entry'."
  (remove-element (entries dir) entry))

(defmethod get-dir-entries ((dir dir-entry) start n)
  "Get directory entries from start to start + n."
  (elements-from (entries dir) start n))

(defmethod rename-dir-entry ((dir dir-entry) old-name (new-dir dir-entry) new-name)
  "Rename file 'old-name' in dir to new-dir with name 'new-name'."
  (let ((entry (get-entry dir old-name)))
    (remove-dir-entry dir old-name)
    (setf (name entry) new-name)
    (setf (parent entry) new-dir)
    (add-entry new-dir entry)))
