
(defmacro %add-callback (name args &body body)
  `(progn
	 (defgeneric ,name (translator ,@args))
	 (defmethod ,name ((translator translator) ,@args)
	   ,@(if (null body)
		   nil 
		   body))))

(%add-callback make-root-node (underlying-stat)
   (make-instance 'node :stat underlying-stat))

(%add-callback pathconf (node user what)
	(case what
	  ((:link-max :max-canon :max-input
		:pipe-buf :vdisable :sock-maxbuf)
	   -1)
	  ((:name-max)
	   1024)
	  ((:chown-restricted :no-trunc)
	   1)
	  ((:prio-io :sync-io :async-io)
	   0)
	  (:filesizebits
		32)))

(%add-callback allow-open (node user flags is-new-p))
(%add-callback get-translator (node))
(%add-callback file-chmod (node user mode))
(%add-callback file-chown (node user uid gid))
(%add-callback file-utimes (node user atime mtime))
(%add-callback dir-lookup (node user filename))
(%add-callback create-file (node user filename mode))
(%add-callback number-of-entries (node user) 0)
(%add-callback get-entries (node user start end))
(%add-callback allow-author-change (node user author))
(%add-callback create-directory (node user name mode))
(%add-callback remove-entry (node user name directory-p))
(%add-callback file-read (node user start amount stream))
(%add-callback file-sync (node user wait-p omit-metadata-p))
(%add-callback file-syncfs (user wait-p do-children-p))
(%add-callback file-write (node user offset stream))

(defmacro define-callback (name trans-type args &body body)
  `(defmethod ,name ((translator ,trans-type) ,@args)
	 ,@body))

