
(defun get-send-receive-port ()
  (let ((port (port-allocate :right-receive)))
    (port-insert-right port port :make-send)
    port))
