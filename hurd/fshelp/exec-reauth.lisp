
(in-package :hurd)

(defun exec-reauth (use-suid-p
                     uid
                     use-sgid-p
                     gid
                     auth
                     get-file-ids
                     ports
                     num-ports
                     fds
                     num-fds)

