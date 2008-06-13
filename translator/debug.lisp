
(defun show-table (fun module)
  (let ((module-name (string-downcase (string module))))
	(format t "Table for ~a routines:~%" module-name)
	(funcall fun)
	(format t "~%")))

;; display interface routines table at the C level
(defun show-tables ()
  (show-table #'%get-io-info 'io)
  (show-table #'%get-fs-info 'fs)
  (show-table #'%get-fsys-info 'fsys))

