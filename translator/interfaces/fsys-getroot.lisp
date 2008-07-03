
(defun get-translator-callback (node port)
  (cond
	((not (has-passive-trans (stat node)))
	 :no-such-file)
	(t
	  (let ((translator-path (get-translator *translator* node)))
		(when translator-path
		  (values translator-path
				  (stat-get (stat node) 'uid)
				  (stat-get (stat node) 'gid)))))))

(defcallback fetch-root-callback err
			 ((node-port :pointer)
			  (root-parent :pointer)
			  (flags open-flags-t)
			  (underlying port-pointer)
			  (underlying-type :pointer))
  (let* ((node (get-temporary-data *translator* node-port))
		 (stat (stat node))
		 (iouser (make-iouser :uids (stat-get stat 'uid)
							  :gids (stat-get stat 'gid)))
		 (root-parent-port (mem-ref root-parent :int))
		 (new (new-protid *translator* iouser
								  (make-open node flags
											 :root-parent root-parent-port))))
	(setf (mem-ref underlying 'port) (get-right new))
	(setf (mem-ref underlying-type 'msg-type-name) :make-send)
	t))

(defun unsupported-root-file-p (stat flags)
  (and (or (is-sock-p stat)
		   (is-blk-p stat)
		   (is-chr-p stat)
		   (is-fifo-p stat))
	   (or
		 (is flags 'read) (is flags 'write) (is flags 'exec))))

(def-fsys-interface :fsys-getroot ((fsys port)
								   (reply port)
								   (reply-poly msg-type-name)
								   (dotdot port)
								   (gen-uids :pointer)
								   (gen-uids-count msg-type-number)
								   (gen-gids :pointer)
								   (gen-gids-count msg-type-number)
								   (flags open-flags-t)
								   (retry-type :pointer)
								   (retry-name :pointer)
								   (file port-pointer)
								   (file-poly :pointer))
 (with-accessors ((root-node root)) *translator*
   (when (and root-node
			  (port-exists fsys))
	 (let ((user (make-iouser-mem gen-uids gen-uids-count
								  gen-gids gen-gids-count)))
	   (only flags 'hurd)
	   (block outer-block
		(with-accessors ((root-stat stat) (root-box box)) root-node
		  (when (and (or (has-passive-trans-p root-stat)
						 (box-translated-p root-box))
					 (is flags 'notrans))
			(insert-temporary-data *translator* fsys root-node)
			(let ((ret-fetch (fetch-root root-box fsys dotdot flags
										 gen-uids gen-uids-count
										 gen-gids gen-gids-count
										 #'get-translator-callback (callback fetch-root-callback)
										 retry retry-name file)))
			  (remove-temporary-data *translator* fsys)
			  (unless (eq ret-fetch :no-such-file)
				(if (eq ret-fetch t)
				  (setf (mem-ref file-poly 'msg-type-name) :move-send))
				(return-from outer-block ret-fetch))))
		  (when (unsupported-root-file-p root-stat flags)
			(return-from outer-block nil))
		  (unless (allow-open *translator* root-node user flags t)
			(return-from outer-block :not-permitted))
		  (disable flags 'open-modes)
		  (let ((new (new-protid *translator* user
					 (make-open-node (root *translator*)
												 flags
												 :root-parent dotdot))))
			(port-deallocate dotdot)
			(setf (mem-ref retry-type 'retry-type) :retry-normal)
			(setf (mem-ref file 'port) (get-right new))
			(setf (mem-ref file-poly 'msg-type-name) :make-send)
			(lisp-string-to-foreign "" retry-name 1)
			t)))))))
