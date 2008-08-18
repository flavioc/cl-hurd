
(defpackage :zip-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :flexi-streams
        :zip))

(in-package :zip-translator)

;;
;; This is a simple zip translator.
;; Right now it supports file and directory listing.
;;

(unless (= (length ext:*args*) 1)
  (error "You must pass a zip file as an argument."))

(defconstant +file+ (first ext:*args*))

(defvar *zip* (open-zipfile +file+) "The zip handle.")

(unless *zip*
  (error "Error while opening the zip file ~a" +file+))

(defconstant +seq-cache-size+ 10 "Number of reads before disposing the extract array sequence.")

(defclass zip-translator (tree-translator)
  ((timestamp :initform nil
              :accessor timestamp
              :initarg :timestamp))
  (:documentation "Zip translator."))

(defclass dirty-entry ()
  ((dirty :initform nil
          :accessor dirty)))

(defclass zip-entry (entry dirty-entry)
  ((name :initarg :name
         :accessor name
         :documentation "Used to access the zip-entry from *zip*.")
   (entry :initarg :entry
          :accessor entry
          :documentation "The zip entry associated with this file.")
   (data-sequence :initarg :data
                  :initform nil
                  :accessor data
                  :documentation "The zip data associated with this file.")
   (number-reads :initform +seq-cache-size+
                 :accessor number-reads
                 :documentation "Count of reads."))
  (:documentation "Extends entry with a zip-entry."))

(defclass zip-dir-entry (dir-entry dirty-entry)
  ((name :initarg :name
         :initform nil
         :accessor name)))

(defmethod print-object ((entry zip-dir-entry) stream)
  (format stream "#<zip-dir-entry name=~s>" (name entry)))

(defmethod print-object ((entry zip-entry) stream)
  (format stream "#<zip-entry name=~s>" (name entry)))

(defun %get-entry-sequence (entry)
  (let ((data-stream
          (make-in-memory-output-stream
            :element-type '(unsigned-byte 8))))
    (zipfile-entry-contents entry data-stream)
    (get-output-stream-sequence data-stream)))

(define-callback read-file zip-translator
                 (node user start amount stream)
  (unless (has-access-p node user :read)
    (return-from read-file :permission-denied))
  (when (is-dir-p (stat node))
    (return-from read-file :is-a-directory))
  (unless (data node)
    ; Get data sequence
    (setf (data node) (%get-entry-sequence (entry node))))
  (decf (number-reads node))
  (let* ((size (stat-get (stat node) 'st-size))
         (size-res (- size start)))
    (cond
      ((not (plusp size-res)) t)
      (t
        (let* ((total (min size-res amount))
               (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)
        t)))))

(define-callback report-access zip-translator
                 (node user)
  (when (has-access-p node user :read)
    '(:read)))

(define-callback refresh-node zip-translator
                 (node user)
  (declare (ignore node user))
  (with-port-deallocate (port (file-name-lookup +file+ :flags '(:read :notrans)))
    (let* ((stat (io-stat port))
           (new-timestamp (stat-get stat 'st-mtime)))
      (when (time-value-newer-p new-timestamp (timestamp translator))
        ; Mark every node as un-visited.
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (declare (ignore name))
                                (setf (dirty node) nil)
                                t))
        (setf *zip* (open-zipfile +file+))
        (do-zipfile-entries (name entry *zip*)
                            (update-zip-file (root *translator*) (split-path name) entry))
        ; Now remove the nodes we have not visited during the update.
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (cond
                                  ((dirty node) t) ; Keep going down there
                                  (t
                                    (remove-dir-entry (parent node)
                                                      name)
                                    nil))))
        (setf (timestamp translator) new-timestamp)))))

(define-callback report-no-users zip-translator
                 (node)
  (when (typep node 'zip-entry)
    ; We don't need this anymore
    (when (or (data node)
               (<= (number-reads node)))
      (setf (data node) nil)
      (setf (number-reads node) +seq-cache-size+))))

(defun %create-zip-file (parent entry)
  "Create a new zip entry."
  (let ((stat (make-stat (stat parent)
                         :size (zipfile-entry-size entry)
                         :type :reg))
        (name (zipfile-entry-name entry)))
    (clear-perms stat :exec)
    (make-instance 'zip-entry
                   :stat stat
                   :parent parent
                   :name name
                   :entry entry)))

(defun %create-zip-dir (parent name)
  "Create a new zip directory."
  (make-instance 'zip-dir-entry
                 :stat (make-stat (stat parent))
                 :name name
                 :parent parent))

(defun %update-file (node zip-entry)
  ; Reset any extracted data.
  (setf (data node) nil
        (stat-get (stat node) 'st-size) (zipfile-entry-size zip-entry)
        (entry node) zip-entry
        (number-reads node) +seq-cache-size+))

(defun update-zip-file (node name zip-entry)
  (let* ((name-rest (rest name))
         (this-name (first name))
         (final-p (null name-rest)))
    (if (string= this-name "")
      (return-from update-zip-file nil))
    (let ((entry (get-entry node this-name)))
      (cond
        (entry
          (cond
            (final-p
              (cond
                ((typep entry 'zip-dir-entry)
                 (remove-dir-entry node this-name)
                 (setf entry (add-entry node (%create-zip-file node zip-entry) this-name)))
                (t
                  (%update-file entry zip-entry))))
            (t
              (when (typep entry 'zip-entry)
                (remove-dir-entry node this-name)
                (setf entry (%create-zip-dir node this-name)))
              (update-zip-file entry name-rest zip-entry))))
        (t
          (setf entry (add-entry node
                                 (if final-p
                                   (%create-zip-file node zip-entry)
                                   (%create-zip-dir node this-name))
                                 this-name))
          (unless final-p
            (update-zip-file entry name-rest zip-entry))))
      (setf (dirty entry) t))))

(defun add-zip-file (node name zip-entry)
  "Recursively using name as a path list add into 'node' a new 'zip-entry'."
  (let* ((name-rest (rest name))
         (this-name (first name))
         (final-p (null name-rest)))
    (if (string= this-name "")
      ;; Last node was a directory and it is already created.
      (return-from add-zip-file nil))
    (let ((entry (get-entry node this-name)))
      (cond
        (entry
          (unless final-p
            (add-zip-file entry name-rest zip-entry)))
        (t
          (let ((new-dir (add-entry node
                                    (if final-p
                                      (%create-zip-file node zip-entry)
                                      (%create-zip-dir node this-name))
                                    this-name)))
            (unless final-p
              (add-zip-file new-dir name-rest zip-entry))))))))

(define-callback fill-root-node zip-translator
                 ((node dir-entry))
  "Add all entries found on the zip file."
  (do-zipfile-entries (name entry *zip*)
                      (add-zip-file node (split-path name) entry)))

(defun main ()
  (with-port-deallocate (port (file-name-lookup +file+ :flags '(:read)))
    (run-translator (make-instance 'zip-translator
                                   :timestamp (stat-get (io-stat port) 'st-mtime)
                                   :name "zip-translator"))))

(main)

