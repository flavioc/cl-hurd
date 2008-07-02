
(define-helper-library fetch-root)

(defcfun ("helper_fetch_root" %helper-fetch-root)
		 err
		 (dotdot port)
		 (fetch-root-callback :pointer)
		 (node-port port)
		 (uid :unsigned-int)
		 (gid :unsigned-int)
		 (argz :string)
		 (argz-len :unsigned-int)
		 (control-port port-pointer))

(defun helper-fetch-root (dotdot
						   fetch-root-callback
						   node-port
						   uid
						   gid
						   path)
  (with-foreign-ptr (control (foreign-type-size 'port))
		(let ((return-code (%helper-fetch-root dotdot callback2
							node-port uid gid path (+ 1 (length path)))))
		  (select-error return-code (mem-ref control 'port)))))

(defun try-start-translator (box dotdot fetch-root-callback
								 node-port path uid gid)
  (multiple-value-bind (ret err)
	(control-port (helper-fetch-root dotdot
									 fetch-root-callback
									 node-port
									 uid gid
									 path))
	(set-starting box nil)
	(set-wanted box nil)
	(if err
	  (return-from try-start-translator err))
	(if (port-valid ret)
	  ret
	  :translator-died)))

(defun fetch-root (box port
					   dotdot
					   flags
					   uids uids-count
					   gids gids-count
					   get-translator-callback
					   fetch-root-callback
					   retry
					   retry-name
					   retry-port)
  (unless (box-translated-p box)
	 (if (box-starting-p box)
	   (set-wanted box t))
	 (set-starting box t)
	 (multiple-value-bind (path uid gid) (funcall get-translator-callback (node box) port)
	   (unless path
		 (set-starting box nil)
		 (set-wanted box nil)
		 (return-from fetch-root path)) ; return error
	   (let ((control-port (try-start-translator box dotdot
												 fetch-root-callback
												 port
												 path
												 uid
												 gid)))
		 (unless (port-valid control-port)
		   (set-starting box nil)
		   (set-wanted box nil)
		   (return-from fetch-root control-port)) ; error
		 (setf (active box) control-port))))
  (let ((control (active box)))
	(port-mod-refs control :right-send 1)
	(let ((root (fsys-getroot control dotdot :copy-send
								 uids uids-count
								 gids gids-count
								 flags retry retry-name)))
	  (when (null root)
		(setf (active box) nil)
		(port-deallocate control)
		(return-from fetch-root nil))
	  (setf (mem-ref retry-port 'port) root)
	  t)))
