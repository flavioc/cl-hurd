
(in-package :hurd-translator)

(def-io-interface :io-reauthenticate ((file port)
                                      (rend-port port))
  (with-lookup protid file
    (let ((new-protid (new-protid *translator*
                                  (get-user protid)
                                  (open-node protid))))
 ;     (warn "going to reauth")
      (with-port-deallocate (new-right (get-send-right new-protid))
        (with-port-deallocate (rendezvous rend-port)
          (let ((new-user (user-reauth +auth-server+ rendezvous new-right t)))
;            (warn "new-user ~s" new-user)
            (when new-user
              (setf (user new-protid) new-user))
            t))))))
