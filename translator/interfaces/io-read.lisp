
(defun %is-minus-one-p (val) (= val #xffffffff))
(defun %use-current-offset-p (offset) (%is-minus-one-p offset))

(defun %io-read (open-node node user amount offset)
  (let* ((current-offset-p (%use-current-offset-p offset))
		 (any-amount-p (%is-minus-one-p amount))
		 (start (if current-offset-p
				  (file-offset open-node)
				  offset)))
	(let* ((out-stream (make-in-memory-output-stream))
		   (ret-read (read-file *translator* node user start (if any-amount-p nil amount) out-stream)))
	  (with-cleanup (close out-stream)
		 (when ret-read
		   (let* ((data-read (get-output-stream-sequence out-stream))
				  (total-read (1+ (length data-read))))
			 (if current-offset-p
			   (incf (file-offset open-node) total-read))
			 (values data-read total-read)))))))

(def-io-interface :io-read ((port port)
							(data :pointer)
							(datalen :pointer)
							(offset off-t)
							(amount msg-type-number))
  (with-lookup protid port
	;(warn "io-read offset ~s amount ~s" offset amount)
	(block io-read
		   (let ((open-node (open-node protid))
				 (node (get-node protid))
				 (user (get-user protid)))
			 (unless (is (flags open-node) 'read)
			   (return-from io-read :bad-fd))
;			 (when (< offset -1)
;			   (warn "offset < -1")
;			   (return-from io-read :invalid-argument))
			 (when (and (not (%use-current-offset-p offset)) (> offset (stat-get (stat node) 'size)))
			   (return-from io-read :invalid-argument))
			 (multiple-value-bind (data-read total)
			   (%io-read open-node node user
						 amount
						 offset)
			 (when (null data-read)
			   (setf (mem-ref datalen 'msg-type-number) 0)
			   (return-from io-read t))
			 (let ((needs-allocate-p (> total (mem-ref datalen 'msg-type-number))))
			   (when needs-allocate-p
				 (setf (mem-ref data :pointer)
					   (mmap (make-pointer 0)
							 total
							 '(:prot-read :prot-write)
							 '(:map-anon)
							 0 0)))
			   (setf (mem-ref datalen 'msg-type-number) total)
			   (lisp-string-to-foreign data-read
									   (mem-ref data :pointer)
									   total)
			   t))))))