
(in-package :hurd-example-translators)

;;
;; This is a simple zip translator.
;; Right now it supports file and directory listing.
;;

(defvar *zip* (open-zipfile "tmp.zip") "The zip handle.")

(defclass zip-translator (tree-translator)
  ((name :initform "zip-translator"
         :documentation "Translator name"))
  (:documentation "Zip translator."))

(defclass zip-entry (entry)
  ((data-sequence :initarg :data
                  :accessor data
                  :documentation "The zip data associated with this file."))
  (:documentation "Extends entry with a zip-entry."))

(define-callback allow-open zip-translator (node user flags is-new-p) t)

(define-callback file-read zip-translator
				 (node user start amount stream)
  (let* ((size (stat-get (stat node) 'size))
         (size-res (- size start)))
    (cond
      ((not (plusp size-res))
       nil)
      (t
        (let* ((total (if (null amount)
                        size-res
                        (min size-res amount)))
               (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)
        (if (eq end size)
          (write-byte #xA0 stream))
        t)))))

;; XXX
(define-callback file-write zip-translator
				 (node user offset stream)
  t)

(defun %create-zip-file (name parent entry)
  "Create a new zip entry."
  (let ((data-stream
          (make-in-memory-output-stream
            :element-type '(unsigned-byte 8)))
        seq
        stat)
    (zipfile-entry-contents entry data-stream)
    (setf seq (get-output-stream-sequence data-stream))
    (setf stat (make-stat (stat parent)
                          :size (length seq)))
    (clear-perms stat 'exec) ; Clear exec permissions.
    (let ((obj (make-instance 'zip-entry
                              :stat stat
                              :name name
                              :parent parent
                              :data seq)))
      (setup-entry obj)
      obj)))

(defun %create-zip-dir (name parent)
  "Create a new zip directory."
  (make-dir name (make-stat (stat parent)) parent))

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
                                      (%create-zip-file this-name node zip-entry)
                                      (%create-zip-dir this-name node)))))
            (unless final-p
              (add-zip-file new-dir name-rest zip-entry))))))))

(defmethod fill-root-node ((translator zip-translator) (node dir-entry))
  "Add all entries found on the zip file."
  (do-zipfile-entries (name entry *zip*)
                      (add-zip-file node (split-path name) entry)))

(defvar *zip-translator*
  (make-translator 'zip-translator) "The translator.")

;; Startup the translator.
(run-translator *zip-translator*)

;; to be removed
;;
(define-callback file-utimes zip-translator
				 (node user atime mtime)
  (when atime
    (setf (stat-get (stat node) 'atime) atime))
  (when mtime
    (setf (stat-get (stat node) 'mtime) mtime))
  t)

(define-callback file-chmod zip-translator
				 (node user mode)
  (set-perms-if (stat node)
                (has-perms-p mode 'read 'others)
                'read 'others))

(define-callback file-chown zip-translator
				 (node user uid gid)
  (when (valid-id-p uid)
    (setf (stat-get (stat node) 'uid) uid))
  (when (valid-id-p gid)
    (setf (stat-get (stat node) 'gid) gid))
  t)

(define-callback allow-author-change zip-translator
				 (node user author)
  (warn "changing author in node ~s to ~s" (name node) author)
  t)

