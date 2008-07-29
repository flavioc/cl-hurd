
(in-package :hurd-translator)

(defun port-exists-p (port)
  "Checks port existence on the actual translator."
  (bucket-has-port-p (port-bucket *translator*) port))
