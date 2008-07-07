
(in-package :hurd-translator)

(defmethod propagate-read-to-execute ((stat stat))
  "Enables the execute permission bit if the read bit is on."
  (if (has-perms-p stat 'read 'owner)
    (set-perms stat 'exec 'owner))
  (if (has-perms-p stat 'read 'group)
    (set-perms stat 'exec 'group))
  (if (has-perms-p stat 'read 'others)
    (set-perms stat 'exec 'others))
  t)

(defun port-exists-p (port)
  "Checks port existence on the actual translator."
  (has-port (port-bucket *translator*) port))
