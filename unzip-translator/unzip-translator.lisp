
(defpackage :unzip-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :hurd-streams
        :flexi-streams
        :trivial-gray-streams
        :zip))

(in-package :unzip-translator)

;;
;; This is a simple unzip translator.
;; Right now it supports file and directory listing.
;;

(defconstant +seq-cache-size+ 10 "Number of reads before disposing the extract array sequence.")

(defvar *must-save* nil)

(defclass zip-translator (tree-translator)
  ((timestamp :initform nil
              :accessor timestamp
              :initarg :timestamp)
   (underlying-stream :initform nil
                      :accessor underlying-stream
                      :initarg :stream))
  (:documentation "Zip translator."))

(defclass dirty-entry ()
  ((dirty :initform nil
          :accessor dirty)))

(defclass zip-entry (entry dirty-entry)
  ((name :initarg :name
         :accessor name)
   (entry :initarg :entry
          :accessor entry
          :initform nil
          :documentation "The zip entry associated with this file.")
   (to-write :initform nil
             :initarg :to-write
             :accessor to-write)
   (data-sequence :initarg :data
                  :initform nil
                  :accessor data
                  :documentation "The zip data associated with this file.")
   (number-reads :initform +seq-cache-size+
                 :accessor number-reads
                 :documentation "Count of reads."))
  (:documentation "Extends entry with a zip-entry."))

(defmethod to-write-p ((entry zip-entry))
  (to-write entry))

(defmethod activate-write ((entry zip-entry))
  (setf (to-write entry) t
        (entry entry) nil
        *must-save* t))

(defmethod name ((foo dir-entry))
  (declare (ignore foo))
  nil)

(defclass zip-dir-entry (dir-entry dirty-entry)
  ((name :initarg :name
         :initform nil
         :accessor name)))

(define-callback chown-file zip-translator
                 (node user uid gid)
  (declare (ignore node user uid gid))
  nil)

(define-callback chmod-file zip-translator
                 (node user mode)
  (declare (ignore node user mode))
  nil)

(define-callback create-hard-link zip-translator
                 (dir user file name)
  (declare (ignore dir user file name))
  nil)

(define-callback allow-link-p zip-translator
                 (node user)
  (declare (ignore node user))
  nil)

(define-callback create-symlink zip-translator
                 (node user target)
  (declare (ignore node user target))
  nil)

(defun %get-entry-sequence (entry)
  (let ((data-stream
          (make-in-memory-output-stream
            :element-type '(unsigned-byte 8))))
    (zipfile-entry-contents entry data-stream)
    (get-output-stream-sequence data-stream)))

(defun extract-node (node)
  (setf (data node) (%get-entry-sequence (entry node))))

(define-callback read-file zip-translator
                 (node user start amount stream)
  (unless (has-access-p node user :read)
    (return-from read-file :permission-denied))
  (when (is-dir-p (stat node))
    (return-from read-file :is-a-directory))
  (unless (data node)
    (extract-node node))
  (unless (to-write-p node)
    (decf (number-reads node)))
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

(defun create-adjustable-array ()
  (make-array 0
              :fill-pointer 0
              :adjustable t
              :element-type '(unsigned-byte 8)))

(define-callback create-file zip-translator
                 (node user filename mode)
  (unless (has-access-p node user :write)
    (return-from create-file nil))
  (let ((entry (make-instance 'zip-entry
                              :name filename
                              :to-write t
                              :data (create-adjustable-array)
                              :stat (make-stat (stat node)
                                               :mode mode
                                               :size 0
                                               :type :reg)
                              :parent node)))
    (setf *must-save* t)
    (add-entry node entry filename)
    entry))

(defun ensure-write-data (node &optional new-size)
  (cond
    ((and (null (data node))
          (or (null new-size)
              (plusp new-size)))
     (extract-node node))
    ((null (data node))
     (setf (data node) (create-adjustable-array))))
  (activate-write node))

(define-callback file-change-size zip-translator
                 (node user new-size)
  (when (is-dir-p (stat node))
    (return-from file-change-size :is-a-directory))
  (when (is-owner-p node user)
    (ensure-write-data node new-size)
    (adjust-array (data node) new-size :fill-pointer t)
    (setf (stat-get (stat node) 'st-size) new-size)
    t))

(defun %read-sequence (stream amount)
  (let ((arr (make-array amount
                         :element-type '(unsigned-byte 8))))
    (read-sequence arr stream)
    arr))

(define-callback write-file zip-translator
                 (node user offset stream amount)
  (unless (has-access-p node user :write)
    (return-from write-file nil))
  (when (is-dir-p (stat node))
    (return-from write-file :is-a-directory))
  (ensure-write-data node)
  (let* ((size (stat-get (stat node) 'st-size))
         (arr (%read-sequence stream amount))
         (final-size (max (+ amount offset) size)))
    (unless (= final-size size)
      (adjust-array (data node)
                    final-size
                    :fill-pointer t))
    (replace (data node) arr :start1 offset)
    ; Update stat size.
    (setf (stat-get (stat node) 'st-size) final-size)
    t))

(define-callback file-rename zip-translator
                 (user old-dir old-name new-dir new-name)
  (declare (ignore user old-dir old-name))
  (when (call-next-method)
    (let ((new-entry (get-entry new-dir new-name)))
      (when new-entry
        (setf (name new-entry) new-name)
        t))))

(define-callback create-directory zip-translator
                 (node user name mode)
  (when (not-permitted-entries-p name)
    (return-from create-directory nil))
  (unless (is-owner-p node user)
    (return-from create-directory nil))
  (let ((old (get-entry node name)))
    (cond
      (old nil)
      (t
        (setf *must-save* t)
        (add-entry node
                   (make-instance 'zip-dir-entry
                                  :stat (make-stat (stat node) :mode mode)
                                  :name name
                                  :parent node)
                   name)))))

(define-callback refresh-node zip-translator
                 (node user)
  (declare (ignore node user))
  (with-accessors ((underlying-node underlying-node)) translator
    (let* ((stat (io-stat underlying-node))
           (new-timestamp (stat-get stat 'st-mtime)))
      (when (time-value-newer-p new-timestamp (timestamp translator))
        ; Mark every node as un-visited.
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (declare (ignore name))
                                (setf (dirty node) nil)
                                t))
        (with-accessors ((underlying-stream underlying-stream) (root root)) translator
          (setf (stream-file-position underlying-stream) :start)
          (let ((zip-handle (open-zipfile underlying-stream)))
            (do-zipfile-entries (name entry zip-handle)
                                (update-zip-file root (split-path name) entry))))
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
                 ((node zip-entry))
  (unless (to-write-p node)
    ; We don't need this anymore
    (when (or (data node)
               (<= (number-reads node)))
      (setf (data node) nil)
      (setf (number-reads node) +seq-cache-size+))))

(defun %create-zip-file (parent entry name)
  "Create a new zip entry."
  (let ((stat (make-stat (stat parent)
                         :size (zipfile-entry-size entry)
                         :type :reg)))
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
  (unless (to-write-p node)
    (setf (data node) nil
          (stat-get (stat node) 'st-size) (zipfile-entry-size zip-entry)
          (entry node) zip-entry
          (number-reads node) +seq-cache-size+)))

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
                 (setf entry (add-entry node
                                        (%create-zip-file node zip-entry this-name)
                                        this-name)))
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
                                   (%create-zip-file node zip-entry this-name)
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
                                      (%create-zip-file node zip-entry this-name)
                                      (%create-zip-dir node this-name))
                                    this-name)))
            (unless final-p
              (add-zip-file new-dir name-rest zip-entry))))))))

(defmethod zip-stream-file-length ((stream hurd-input-stream))
  (hurd-stream-file-length stream))

(defconstant +unix-to-universal-time+ 2208988800)

(defun unix-to-universal-time (secs)
  (+ secs +unix-to-universal-time+))

(defun get-full-path (node)
  (let ((my-name (name node)))
    (when my-name
      (let* ((parent (parent node))
             (parent-path (get-full-path parent)))
        (if parent-path
          (concatenate-string parent-path "/" my-name)
          my-name)))))

(defun get-write-date (node)
  (unix-to-universal-time (time-value-seconds (stat-get (stat node) 'st-mtime))))

(defgeneric write-zip-node (node writer))

(defmethod write-zip-node ((node zip-entry) writer)
  (let ((path (get-full-path node))
        (node-stream (make-in-memory-input-stream (data node))))
    (write-zipentry writer path node-stream
                    :file-write-date (get-write-date node))))

(defmethod write-zip-node ((node zip-dir-entry) writer)
  (let ((path (concatenate-string (get-full-path node) "/")))
    (write-zipentry writer
                    path
                    (make-concatenated-stream)
                    :file-write-date (get-write-date node))))

(define-callback shutdown zip-translator
                 ()
  (when *must-save*
    (warn "Saving zip file...")
    ; Extract everything first
    (iterate-entries-deep (root translator)
                          (lambda (name node)
                            (declare (ignore name))
                            (when (typep node 'zip-entry)
                              (ensure-write-data node))
                            t))
    (let ((s (make-hurd-output-stream (underlying-node translator))))
      (file-set-size (underlying-node translator) 0)
      (let ((writer (make-zipfile-writer s)))
        (iterate-entries-deep (root translator)
                              (lambda (name node)
                                (declare (ignore name))
                                (write-zip-node node writer)
                                t))
        (zip-write-central-directory writer)
        (force-output s)))))

(define-callback fill-root-node zip-translator
                 ((node dir-entry))
  "Add all entries found on the zip file."
  (let ((zip-handle (open-zipfile (underlying-stream translator))))
    (do-zipfile-entries (name entry zip-handle)
                        (add-zip-file node (split-path name) entry))))

(define-callback make-root-node zip-translator
                 (underlying-node underlying-stat)
  (setf (timestamp translator) (stat-get underlying-stat 'st-mtime)
        (underlying-stream translator) (make-hurd-input-stream underlying-node))
  (call-next-method))

(defun main ()
  (run-translator (make-instance 'zip-translator
                                 :name "zip-translator"
                                 :version (list 0 1 0))
                  :flags '(:notrans :read :write)))

(main)

