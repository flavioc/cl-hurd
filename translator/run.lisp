
(in-package :hurd-translator)

(defmethod inner-run ((translator translator))
  "Run the translator server."
  (let ((*translator* translator))
    (run-server (lambda (port in out)
                  (translator-demuxer in out))
                (port-bucket *translator*))))

(defmethod run-translator ((translator translator))
  "Setup the translator and the run it."
  (when (running-p translator)
    (let* ((under (underlying-node translator))
           (stat (io-stat under)))
      (setf (root translator)
            (make-root-node translator (make-stat stat))))
    (inner-run translator)))

