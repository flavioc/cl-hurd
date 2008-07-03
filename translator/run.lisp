
(defmethod run ((trans translator))
  (let ((*translator* trans))
    (run-server (lambda (port in out) (translator-demuxer in out))
		(port-bucket trans))))

(defun run-translator ((translator translator))
  (when (setup translator)
	(let* ((under (underlying-node translator))
		   (stat (io-stat under)))
	  (setf (root translator)
		  (make-root-node translator (make-stat stat))))
	(run translator)))

