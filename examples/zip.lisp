
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

(assert (= (length ext:*args*) 1))
(defvar *zip* (open-zipfile (first ext:*args*)) "The zip handle.")

(defconstant +seq-cache-size+ 10 "Number of reads before disposing the extract array sequence.")

(defclass zip-translator (tree-translator)
  ()
  (:documentation "Zip translator."))

(defclass zip-entry (entry)
  ((name :initarg :name
         :accessor name
         :documentation "Used to access the zip-entry from *zip*.")
   (entry :initarg :entry
          :accessor entry
          :initform nil
          :documentation "The zip entry associated with this file.")
   (data-sequence :initarg :data
                  :initform nil
                  :accessor data
                  :documentation "The zip data associated with this file.")
   (number-reads :initform +seq-cache-size+
                 :accessor number-reads
                 :documentation "Count of reads."))
  (:documentation "Extends entry with a zip-entry."))

(defclass zip-dir-entry (dir-entry)
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
    (setf (entry node) (get-zipfile-entry (name node) *zip*))
    (setf (data node) (%get-entry-sequence (entry node)))
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

(define-callback report-access zip-translator
                 (node user)
  (when (has-access-p node user :read)
    '(:read)))

(define-callback report-no-users zip-translator
                 (node)
  (when (typep node 'zip-entry)
    ; We don't need these anymore
    (when (and (entry node)
               (<= (number-reads node)))
      (setf (entry node) nil
            (data node) nil)
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
                   :name name)))

(defun %create-zip-dir (parent name)
  "Create a new zip directory."
  (make-instance 'zip-dir-entry
                 :stat (make-stat (stat parent))
                 :name name
                 :parent parent))

(defun add-zip-file (node name zip-entry)
  "Recursively using name as a path list add into 'node' a new 'zip-entry'."
  (let* ((name-rest (rest name))
         (this-name (first name))
         (final-p (null name-rest)))
    (if (string= this-name "")
      ;; Last node was a directory and it is already created.
      (return-from add-zip-file nil))
    (multiple-value-bind (entry found-p)
      (get-entry node this-name)
      (cond
        (found-p
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
  (run-translator (make-instance 'zip-translator
                                 :name "zip-translator")))

(main)

