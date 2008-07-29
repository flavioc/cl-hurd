
(in-package :hurd-translator)

(defun port-exists-p (port)
  "Checks port existence on the actual translator."
  (has-port (port-bucket *translator*) port))
