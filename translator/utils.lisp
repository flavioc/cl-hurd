
(defmethod propagate-read-to-execute ((stat stat))
  (if (has-perms stat 'read 'owner)
	(set-perms stat 'exec 'owner))
  (if (has-perms stat 'read 'group)
	(set-perms stat 'exec 'group))
  (if (has-perms stat 'read 'others)
	(set-perms stat 'exec 'others))
  t)

(defun port-exists (port)
  (has-port (port-bucket *translator*) port))
