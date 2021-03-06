
(in-package :hurd-translator)

(defmethod configure ((translator translator) flags)
  "Gets the bootstrap port to call fsys-startup and installs a new control port into the bucket."
  (with-port-deallocate (bootstrap (task-get-bootstrap-port))
    (let ((port (bucket-add-control-port (port-bucket translator))))
      (with-port-deallocate (right (get-send-right port))
        (setf (slot-value translator 'underlying-node)
              (fsys-startup bootstrap flags right :copy-send))))))

(defmethod inner-run ((translator translator) demuxer)
  "Run the translator server."
  (let ((*translator* translator))
    (run-server (lambda (port in out)
                  (declare (ignore port))
                  (funcall demuxer in out))
                (port-bucket *translator*))))

(defmethod setup-translator ((translator translator) &key flags)
  "Setup the translator to be ready to run."
  (configure translator flags)
  (when (running-p translator)
    (let* ((under (underlying-node translator))
           (stat (io-stat under)))
      (set-trans stat nil)
      (set-root stat t)
      (setf (root translator)
            (make-root-node translator
                            (underlying-node translator)
                            (make-stat stat)))
      t)))

(defmethod run-translator ((translator translator)
                           &key flags (demuxer #'translator-demuxer))
  "Setup the translator and then run it."
  (setup-translator translator :flags flags)
  (inner-run translator demuxer))

(defun calculate-miliseconds (seconds miliseconds)
  "Return total of miliseconds."
  (+ (* 1000 seconds) miliseconds))

(defun wait (&key (seconds 0) (miliseconds 0) (demuxer #'translator-demuxer))
  "Runs the translator server during 'seconds' seconds and 'miliseconds' miliseconds."
  (unless (and (zerop seconds)
               (zerop miliseconds))
    (when (running-p *translator*)
      (run-server (lambda (port in out)
                    (declare (ignore port))
                    (funcall demuxer in out))
                  (port-bucket *translator*)
                  (calculate-miliseconds seconds miliseconds)))))

