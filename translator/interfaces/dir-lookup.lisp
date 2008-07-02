
(defun %create-new-protid (open-node user node flags mode newnode-p)
  (let ((allow (allow-open *translator*
						   node
						   user
						   flags
						   newnode-p)))
	(when allow
	   (disable flags 'non-open-modes)
	   (let* ((new-user (make-iouser :old user))
			  (new-protid (new-protid *translator* new-user
									  (make-open-node node
													  flags
													  :root-parent (root-parent open-node)
													  :shadow-root (shadow-root open-node)
													  :shadow-root-parent (shadow-root-parent open-node)))))
		 (values :retry-normal
				 ""
				 (get-right new-protid)
				 :make-send)))))

(defun %dir-lookup (open-node user node path-ls flags mode)
  (warn "now looking in node ~s" (name node))
  (let ((this-path (first path-ls))
		(rest-path (rest path-ls)))
  (when (string= this-path "") ; this is last path
	(return-from %dir-lookup (%create-new-protid open-node user node flags mode nil)))
  (let ((shadow-root (shadow-root open-node)))
	(cond
	  ((and (or (eq (root *translator*) node)
				(eq shadow-root node))
			(string= this-path ".."))
	   (cond
		 ((eq node shadow-root)
		  (return-from %dir-lookup (values :retry-reauth
										   (join-path rest-path)
										   (root-parent open-node)
										   :copy-send)))
		 ((not (null (get-root-parent dir-protid)))
		  (return-from %dir-lookup (values :retry-reauth
										   (join-path rest-path)
										   (root-parent open-node)
										   :copy-send)))))
	  (t
		(let ((found-node (dir-lookup *translator* node user this-path)))
		  (cond
			((and found-node
				  (is flags 'creat)
				  (is flags 'excl))
			 :file-exists)
			((and (not found-node)
				  (is flags 'creat)
				  (null rest-path))
			 (set-vtx mode nil)
			 (set-spare mode nil)
			 (set-type mode 'reg)
			 (let ((new-node (create-file *translator*
										  node
										  user
										  this-path
										  mode)))
			   (unless new-node
				 (return-from %dir-lookup :not-permitted))
			   (%create-new-protid open-node user new-node flags mode t)))
			((and (null rest-path)
				  found-node)
			 (%create-new-protid open-node user found-node flags mode nil))
			((and rest-path
				  found-node
				  (not (is-dir-p (stat found-node))))
			 :not-directory)
			((not found-node)
			 :no-such-file)
			(t
			  (warn "continue lookup in ~s" (name node))
			  ;; continue lookup
			  (%dir-lookup open-node user found-node rest-path flags mode)))))))))

(def-fs-interface :dir-lookup ((dir-port port)
							   (filename :string)
							   (flags open-flags-t)
							   (mode mode-t)
							   (do-retry :pointer)
							   (retry-name :pointer)
							   (retry-port port-pointer)
							   (retry-port-type :pointer))
  (with-lookup dir-protid dir-port
	(warn "filename to lookup in ~s: ~a~%" (name (get-node dir-protid)) filename)
	(multiple-value-bind (ret-do-retry
						   ret-retry-name
						   ret-retry-port
						   ret-retry-port-type)
	  (%dir-lookup (open-node dir-protid)
		           (get-user dir-protid)
				   (get-node dir-protid)
				   (split-path filename)
				   flags
				   mode)
	  (cond
		((null ret-retry-name)
		 ret-do-retry) ;; some error ocurred
		(t
		  (setf (mem-ref do-retry 'retry-type) ret-do-retry)
		  (lisp-string-to-foreign ret-retry-name retry-name (+ 1 (length ret-retry-name)))
		  (setf (mem-ref retry-port 'port) ret-retry-port)
		  (setf (mem-ref retry-port-type 'msg-type-name) ret-retry-port-type)
		  t)))))

