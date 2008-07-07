
(in-package :hurd-translator)

(defmethod run ((translator translator))
  "Run the translator server."
  (let ((*translator* translator))
    (run-server (lambda (port in out) (translator-demuxer in out))
                (port-bucket *translator*))))

(defun run-translator (translator)
  "Setup the translator and the run it."
  (when (setup translator)
    (let* ((under (underlying-node translator))
           (stat (io-stat under)))
      (setf (root translator)
            (make-root-node translator (make-stat stat))))
    (run translator)))

