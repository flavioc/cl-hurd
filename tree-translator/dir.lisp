
(in-package :hurd-tree-translator)

;;
;; This file implements the special directory node and entry node.
;;

(defvar *ino-value* 1)

(defclass entry (node)
  ((parent :initform nil
           :initarg :parent
           :accessor parent
           :documentation "Parent node."))
  (:documentation "An entry, with a name and a parent."))

(defmethod print-object ((node entry) stream)
  "Print an entry to stream."
  (format stream "#<entry node: ")
  (call-next-method)
  (format stream ">"))

(defclass inner-entry ()
  ((node :initarg :node
         :accessor node)
   (name :initarg :name
         :accessor name)))

(defun make-inner-entry (node name)
  (make-instance 'inner-entry :node node :name name))

(defclass dir-entry (entry)
  ((entries :initform (make-sorted-container #'string< #'name)
            :accessor entries
            :documentation "The directory entries."))
  (:documentation "A special entry: the directory entry."))

(defun %new-ino-val (stat)
  "Sets and increments the ino value of a stat struct."
  (setf (stat-get stat 'ino) (incf *ino-value*)))

(defmethod add-entry ((dir dir-entry) (entry entry) (name string))
  "Adds a new entry to the directory node 'dir'."
  (let ((found (get-entry dir name)))
    (cond
      (found
        found)
      (t
        (incf (stat-get (stat dir) 'nlink)) ; New entry.
        (insert-element (entries dir)
                        (make-inner-entry entry name))
        entry))))

(defmethod setup-entry ((entry entry))
  "Changes some node information to sane defaults."
  (%new-ino-val (stat entry))
  (setf (stat-get (stat entry) 'nlink) 1)
  entry)

(defmethod setup-entry ((entry dir-entry))
  "Changes some node information to sane defaults on directories."
  (set-type (stat entry) :dir)
  (%new-ino-val (stat entry))
  ; nlink represents number of objects in a directory
  (setf (stat-get (stat entry) 'nlink) 2)
  entry)

(defmethod initialize-instance :after ((entry entry) &key)
  (setup-entry entry))

(defmethod dir-size ((dir dir-entry))
  "Returns number of entries in a directory."
  (+ 2 ; Entries "." and "..".
     (count-elements (entries dir))))

(defmethod get-entry ((dir dir-entry) (entry string))
  "Gets an entry from a directory based on the filename 'entry'."
  (let ((found (get-element (entries dir) entry)))
    (if found
      (node found)
      nil)))

(defmethod get-entry ((foo entry) (entry string))
  nil)

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
    (setf (parent entry) new-dir)
    (add-entry new-dir entry new-name)))
