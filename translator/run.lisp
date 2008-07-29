
(in-package :hurd-translator)

(defmethod inner-run ((translator translator))
  "Run the translator server."
  (let ((*translator* translator))
    (run-server (lambda (port in out)
                  (declare (ignore port))
                  (translator-demuxer in out))
                (port-bucket *translator*))))

(defmethod run-translator ((translator translator))
  "Setup the translator and the run it."
  (when (running-p translator)
    (let* ((under (underlying-node translator))
           (stat (io-stat under)))
      (set-trans stat nil)
      (set-root stat t)
      (setf (root translator)
            (make-root-node translator (make-stat stat))))
    (inner-run translator)))

(defun calculate-miliseconds (seconds miliseconds)
  (+ (* 1000 seconds) miliseconds))

(defun wait (&key (seconds 0) (miliseconds 0))
  (unless (and (zerop seconds)
               (zerop miliseconds))
    (run-server (lambda (port in out)
                  (declare (ignore port))
                  (translator-demuxer in out))
                (port-bucket *translator*)
                (calculate-miliseconds seconds miliseconds))))
