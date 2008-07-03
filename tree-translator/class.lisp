
(defclass tree-translator (translator)
  ((name :initform "tree-translator")))

(define-callback make-root-node tree-translator
				 (underlying-stat)
   (when (not (is-dir-p underlying-stat))
	 (propagate-read-to-execute underlying-stat))
   (set-type underlying-stat 'dir)
   (set-trans underlying-stat nil)
   (let ((obj (make-dir "root" underlying-stat)))
	 (fill-root-node translator obj)
	 obj))

(defmethod fill-root-node ((translator tree-translator) (root dir-entry))
  nil)

(define-callback dir-lookup tree-translator
				 (node user filename)
  (warn "dir-look ~s in ~s" filename (name node))
  (cond
	((string= filename ".")
	 node)
	((string= filename "..")
	 (parent node))
	(t
	  (get-entry node filename))))

(define-callback number-of-entries tree-translator
				 (node user)
  (dir-size node))

(define-callback get-entries tree-translator
				(node user start end)
  (let* ((all (loop for value being the hash-values of (entries node)
				   using (hash-key key)
				   collect (make-dirent key value)))
		 (sorted (sort all (lambda (a b) (string< (name a) (name b))))))
	(subseq sorted start (1+ end))))

(define-callback create-directory tree-translator
				 (node user name mode)
  (let ((new-stat (make-stat (stat node))))
	(setf (stat-get new-stat 'mode) mode)
	(add-entry node (make-dir name new-stat node))))

(define-callback remove-entry tree-translator
				 (node user name directory-p)
  (remove-dir-entry node name))

(define-callback read-file tree-translator
				 (node user start amount stream)
				 nil)
