(defvar *zip* (open-zipfile "tmp.zip"))

(defclass zip-translator (tree-translator)
  ((name :initform "zip-translator")))

(defclass zip-entry (entry)
  ((zip-entry :initarg :zip
			  :accessor zip-entry)))

(defparameter *zip-translator* (create-translator 'zip-translator))

(define-callback allow-open zip-translator (node user flags is-new-p) t)

(define-callback read-file zip-translator
				 (node user start amount stream)
 (if (> start (stat-get (stat node) 'size))
   nil
   (progn
	 (write-sequence (string-to-octets "ola") stream)
	 t)))

(defun create-zip-file (name parent entry)
  (let ((obj (make-instance 'zip-entry
							:stat (make-stat (stat parent)
											 :size (zipfile-entry-size entry))
							:name name
							:parent parent
							:zip entry)))
	(setup-entry obj)
	obj))

(defun create-zip-dir (name parent)
  (make-dir name (make-stat (stat parent)) parent))

(defun add-zip-file (node name zip-entry)
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
									  (create-zip-file this-name node zip-entry)
									  (create-zip-dir this-name node)))))
			(unless final-p
			  (add-zip-file new-dir name-rest zip-entry))))))))

(defmethod fill-root-node ((translator zip-translator) (node dir-entry))
  (do-zipfile-entries (name entry *zip*)
	 (add-zip-file node (split-path name) entry)))

;; startup the translator
(run-translator *zip-translator*)

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
				(has-perms mode 'read 'others)))

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

