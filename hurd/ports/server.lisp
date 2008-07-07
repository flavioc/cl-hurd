
(in-package :hurd)

(defun run-server (demuxer bucket &optional (timeout 0))
  "Runs a new server on a bucket with demuxer 'demuxer'."
  (set-demuxer demuxer)
  (loop until (eq (msg-server-timeout #'portset-demuxer
                                      (port-set bucket)
                                      timeout)
                  :rcv-timed-out)))
