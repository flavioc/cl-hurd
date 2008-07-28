
(in-package :hurd-example-translators)

;;
;; This is a simple zip translator.
;; Right now it supports file and directory listing.
;;

(assert (= (length ext:*args*) 1))
(defvar *zip* (open-zipfile (first ext:*args*)) "The zip handle.")

(defclass zip-translator (tree-translator)
  ((name :initform "zip-translator"
         :documentation "Translator name"))
  (:documentation "Zip translator."))

(defclass zip-entry (entry)
  ((name :initarg :name
         :accessor name)
   (data-sequence :initarg :data
                  :initform (make-array 0)
                  :accessor data
                  :documentation "The zip data associated with this file."))
  (:documentation "Extends entry with a zip-entry."))

(defclass zip-dir-entry (dir-entry)
  ((name :initarg :name
         :initform nil
         :accessor name)))

(defmethod print-object ((entry zip-dir-entry) stream)
  (format stream "#<zip-dir-entry name=~s>" (name entry)))

(defmethod print-object ((entry zip-entry) stream)
  (format stream "#<zip-entry name=~s>" (name entry)))

(define-callback file-read zip-translator
                 (node user start amount stream)
  (declare (ignore user))
  (let* ((size (stat-get (stat node) 'size))
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
  (when (has-access-p node user 'read)
    '(:read)))

(define-callback shutdown zip-translator ()
  (warn "Going down...~%")
  t)

(define-callback options-changed zip-translator
                 ()
  (warn "Options changed:")
  (if (has-translator-option-p (options *translator*) :readonly)
    (warn "READONLY activated"))
  (if (has-translator-option-p (options *translator*) :coolness-level)
    (warn "COOLNESS LEVEL ~s"
          (get-translator-option (options *translator*)
                                 :coolness-level)))
  t)

(defun %create-zip-file (parent entry)
  "Create a new zip entry."
  (let ((data-stream
          (make-in-memory-output-stream
            :element-type '(unsigned-byte 8)))
        seq
        stat)
    (zipfile-entry-contents entry data-stream)
    (setf seq (get-output-stream-sequence data-stream)
          stat (make-stat (stat parent)
                          :size (length seq)
                          :type :reg))
    (clear-perms stat 'exec) ; Clear exec permissions.
    (let ((obj (make-instance 'zip-entry
                              :stat stat
                              :parent parent
                              :data seq)))
      obj)))

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
            ;(warn "new-dir ~s" new-dir)
            (unless final-p
              (add-zip-file new-dir name-rest zip-entry))))))))

(defmethod fill-root-node ((translator zip-translator) (node dir-entry))
  "Add all entries found on the zip file."
  (do-zipfile-entries (name entry *zip*)
                      (add-zip-file node (split-path name) entry)))

(defun main ()
  (run-translator (make-instance 'zip-translator)))

(main)
