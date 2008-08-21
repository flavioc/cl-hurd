
(in-package :hurd-tree-translator)

;;
;; This file implements the special directory node and entry node.
;;

(defvar *ino-value* 1)

(defclass entry (node)
  ((parent :initform nil
           :initarg :parent
           :documentation "Parent node."))
  (:documentation "An entry, with a name and a parent."))

(defmethod set-parent ((entry entry) new-val)
  (unless (null new-val)
    (setf (slot-value entry 'parent)
          (if (tg:weak-pointer-p new-val)
            new-val
            (tg:make-weak-pointer new-val)))))

(defsetf parent set-parent)

(defmethod parent ((entry entry))
  (with-slots ((parent parent)) entry
    (when (and parent
               (tg:weak-pointer-p parent))
      (tg:weak-pointer-value parent))))

(defmethod print-object ((node entry) stream)
  "Print an entry to stream."
  (format stream "#<entry node: ")
  (call-next-method)
  (format stream ">"))

(defclass inner-entry ()
  ((node :initarg :node
         :accessor node
		 :documentation "Saved node.")
   (name :initarg :name
         :accessor name
		 :documentation "Name of the entry.")))

(defun make-inner-entry (node name)
  (make-instance 'inner-entry :node node :name name))

(defclass dir-entry (entry)
  ((entries :initform (make-sorted-container #'string< #'name)
            :accessor entries
            :documentation "The directory entries."))
  (:documentation "A special entry: the directory entry."))

(defun %new-ino-val (stat)
  "Sets and increments the ino value of a stat struct."
  (setf (stat-get stat 'st-ino) (incf *ino-value*)))

(defmethod add-entry ((dir dir-entry) (entry entry) (name string))
  "Adds a new entry to the directory node 'dir'."
  (let ((found (get-entry dir name)))
    (cond
      (found found)
      (t
        (setf (stat-get (stat dir) 'st-mtime) +now-time-value+)
        (setf (stat-get (stat dir) 'st-ctime) +now-time-value+)
        (incf (stat-get (stat dir) 'st-nlink)) ; New entry.
        (assert (>= (stat-get (stat dir) 'st-nlink) 2))
        (incf (stat-get (stat entry) 'st-nlink)) ; New link to this file
        (insert-element (entries dir)
                        (make-inner-entry entry name))
        entry))))

(defmethod setup-entry ((entry entry))
  "Changes some node information to sane defaults."
  (%new-ino-val (stat entry))
  (setf (stat-get (stat entry) 'st-nlink) 0)
  entry)

(defmethod setup-entry ((entry dir-entry))
  "Changes some node information to sane defaults on directories."
  (set-type (stat entry) :dir)
  (%new-ino-val (stat entry))
  ; st-nlink represents number of objects in a directory
  (setf (stat-get (stat entry) 'st-nlink) 1)
  entry)

(defmethod initialize-instance :after ((entry entry) &key)
  (with-slots ((parent parent)) entry
    (when (and parent
               (not (tg:weak-pointer-p parent)))
      ; Install a weak pointer instead
      (setf (slot-value entry 'parent)
            (tg:make-weak-pointer parent))))
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

(defmethod has-entry-p ((dir dir-entry) (entry string))
  "Determines if 'dir' has 'entry'."
  (let ((found (get-element (entries dir) entry)))
    (if found
      t
      nil)))

(defmethod remove-dir-entry ((dir dir-entry) (entry string))
  "Removes a directory entry with name 'entry'."
  (let ((found (get-entry dir entry)))
    (when (and found
               (remove-element (entries dir) entry))
      (assert (> (stat-get (stat dir) 'st-nlink) 2))
      ; Decrease link count.
      (decf (stat-get (stat dir) 'st-nlink))
      (decf (stat-get (stat found) 'st-nlink))
      (setf (stat-get (stat dir) 'st-mtime) +now-time-value+)
      (setf (stat-get (stat dir) 'st-ctime) +now-time-value+)
      t)))

(defmethod get-dir-entries ((dir dir-entry) start n)
  "Get directory entries from start to start + n."
  (setf (stat-get (stat dir) 'st-atime) +now-time-value+)
  (elements-from (entries dir) start n))

(defmethod rename-dir-entry ((dir dir-entry) old-name (new-dir dir-entry) new-name &optional (force-p nil))
  "Rename file 'old-name' in dir to new-dir with name 'new-name'."
  (let ((entry (get-entry dir old-name)))
    (remove-dir-entry dir old-name)
    (setf (parent entry) new-dir)
    (when force-p
      (remove-dir-entry new-dir new-name))
    (add-entry new-dir entry new-name)))

(defmethod iterate-entries ((dir dir-entry) fun)
  "Runs 'fun' for each entry in 'dir'. Arguments are entry name + entry node."
  (iterate-elements (entries dir)
                    (lambda (key value)
                      (funcall fun
                               key
                               (node value)))))

(defmethod iterate-entries-deep ((dir dir-entry) fun)
  "Runs 'fun' for each entry in 'dir', recursively. If 'fun' returns T and the node is a directory, 'fun' will be run for the node's leafs, when it returns NIL the leafs will not be visited."
  (iterate-entries dir
                   (lambda (name node)
                     (when (and (funcall fun name node)
                                (typep node 'dir-entry))
                       (iterate-entries-deep node fun)))))

(defmethod clear-dir ((dir dir-entry))
  "Clear all directory entries."
  (clear-elements (entries dir))
  (setf (stat-get (stat dir) 'st-nlink) 2)
  (setf (stat-get (stat dir) 'st-mtime) +now-time-value+)
  (setf (stat-get (stat dir) 'st-ctime) +now-time-value+)
  t)

