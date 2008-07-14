
(in-package :hurd-translator)

(def-io-interface :io-revoke ((file port))
  (with-lookup protid file
    (let ((this-node (get-node protid))
          (bucket (port-bucket *translator*)))
      (cond
        ((is-owner-p this-node (get-user protid))
         (bucket-iterate bucket
                         (lambda (port)
                           (when (and (typep port 'protid)
                                      (eq this-node (get-node port)))
                             (unless (eq port protid)
                               (remove-port bucket port)))))
         t)
        (t
          :not-permitted)))))
