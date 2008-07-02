
(defmethod run ((trans translator))
  (let ((*translator* trans))
    (run-server (lambda (port in out) (translator-demuxer in out))
		(port-bucket trans))))

(defclass example-translator (translator)
  ((name :initform "example-translator")))

(defparameter *mytranslator* (create-translator 'example-translator))

(define-callback file-chmod example-translator
				 (node user mode)
  (set-perms-if (stat node) 'read 'others
				(has-perms mode 'read 'others)))

(define-callback file-chown example-translator
				 (node user uid gid)
  (when (valid-id-p uid)
	(setf (stat-get (stat node) 'uid) uid))
  (when (valid-id-p gid)
	(setf (stat-get (stat node) 'gid) gid))
  t)

(define-callback file-utimes example-translator
				 (node user atime mtime)
  (when atime
	(setf (stat-get (stat node) 'atime) atime))
  (when mtime
	(setf (stat-get (stat node) 'mtime) mtime))
  t)

(define-callback allow-open example-translator
				 (node user flags is-new-p)
  t)

(define-callback make-root-node example-translator
				 (underlying-stat)
   (set-type underlying-stat 'dir)
   (set-trans underlying-stat nil)
   (when (not (is-dir-p underlying-stat))
	 (propagate-read-to-execute underlying-stat))
   (let ((obj (make-dir "foo" underlying-stat)))
	 (fill-root-node obj)
	 obj))

(define-callback dir-lookup example-translator
				 (node user filename)
  (warn "dir-look ~s in ~s" filename (name node))
  (cond
	((string= filename ".")
	 node)
	((string= filename "..")
	 (parent node))
	(t
	  (let ((result (get-entry node filename)))
		result))))

(define-callback number-of-entries example-translator
				 (node user)
  (warn "number of entries in ~s: ~s" (name node) (dir-size node))
  (dir-size node))

(define-callback get-entries example-translator
				(node user start end)
  (let* ((all (loop for value being the hash-values of (entries node)
				   using (hash-key key)
				   collect (make-dirent key value)))
		 (sorted (sort all (lambda (a b) (string< (name a) (name b))))))
	(subseq sorted start (1+ end))))

(define-callback allow-author-change example-translator
				 (node user author)
  (warn "changing author in node ~s to ~s" (name node) author)
  t)

(define-callback create-directory example-translator
				 (node user name mode)
  (let ((new-stat (make-stat (stat node))))
	(setf (stat-get new-stat 'mode) mode)
	(add-entry node (make-dir name new-stat node))))
	
(defun fill-root-node (node)
  (let ((a-dir (add-entry node (make-dir "a" (make-stat (stat node)) node))))
	(add-entry a-dir (make-entry "d" (make-stat (stat a-dir)) a-dir)))
  (add-entry node (make-entry "c" (make-stat (stat node)) node))
  (add-entry node (make-entry "b" (make-stat (stat node)) node))
  (add-entry node (make-entry "olacrl" (make-stat (stat node)) node)))

(define-callback remove-entry example-translator
				 (node user name directory-p)
  (remove-dir-entry node name))

(when (setup *mytranslator*)
  (let* ((under (underlying-node *mytranslator*))
		 (stat (io-stat under)))
	(setf (root *mytranslator*)
		  (make-root-node *mytranslator* (make-stat stat))))
  (run *mytranslator*))

;; example:
;; (defclass zip-translator ()
;;   ((name :initform "zip-translator")
;;    (version :initform "1")))

