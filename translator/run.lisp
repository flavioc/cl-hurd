
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

(defun calculate-miliseconds (seconds miliseconds)
  (+ (* 1000 seconds)
     miliseconds))

(defun wait (&key (seconds 0) (miliseconds 0))
  (warn "waiting for ~s ~s" seconds miliseconds)
  (unless (and (zerop seconds)
               (zerop miliseconds))
    (run-server (lambda (port in out)
                  (translator-demuxer in out))
                (port-bucket *translator*)
                (calculate-miliseconds seconds miliseconds))))
