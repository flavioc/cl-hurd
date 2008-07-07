
(in-package :hurd-translator)

(defun show-table (fun module)
  "Shows a routine table from the module 'module' using 'fun' to show it."
  (let ((module-name (string-downcase (string module))))
	(format t "Table for ~a routines:~%" module-name)
	(funcall fun)
	(format t "~%")))

(defun show-tables ()
  "Display interface routines table at the C level."
  (show-table #'%get-io-info 'io)
  (show-table #'%get-fs-info 'fs)
  (show-table #'%get-fsys-info 'fsys))

