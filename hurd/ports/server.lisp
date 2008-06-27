
(defun run-server (demuxer bucket &optional (timeout 0))
  (set-demuxer demuxer)
  (loop until (eq (msg-server-timeout (callback portset-demuxer)
				      (port-set bucket))
		  :rcv-timed-out)))
