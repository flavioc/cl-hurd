
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
  ((zip-entry :initarg :zip
              :accessor zip-entry
              :documentation "The zip-entry associated with this node."))
  (:documentation "Extends entry with a zip-entry."))

(define-callback allow-open zip-translator (node user flags is-new-p) t)

;; XXX
(define-callback file-read zip-translator
				 (node user start amount stream)
  (if (> start (stat-get (stat node) 'size))
    nil
    (progn
      (write-sequence (string-to-octets "ola") stream)
      t)))

;; XXX
(define-callback file-write zip-translator
				 (node user offset stream)
  t)

(defun %create-zip-file (name parent entry)
  "Create a new zip entry."
  (let ((obj (make-instance 'zip-entry
                            :stat (make-stat (stat parent)
                                             :size (zipfile-entry-size entry))
                            :name name
                            :parent parent
                            :zip entry)))
    (setup-entry obj)
    obj))

(defun %create-zip-dir (name parent)
  "Create a new zip directory."
  (make-dir name (make-stat (stat parent)) parent))

(defun add-zip-file (node name zip-entry)
  "Recursively using name as a path list add into 'node' a new 'zip-entry'."
  (let* ((name-rest (rest name))
         (this-name (first name))
         (final-p (null name-rest)))
    (if (string= this-name "")
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

(defvar *zip-translator* (make-translator 'zip-translator) "The translator.")

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
  (set-perms-if (stat node) 'read 'others
				(has-perms-p mode 'read 'others)))

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

