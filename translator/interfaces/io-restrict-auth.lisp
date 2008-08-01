
(in-package :hurd-translator)

(def-io-interface :io-restrict-auth ((file port)
                                     (new-port port-pointer)
                                     (new-port-type :pointer)
                                     (uids :pointer)
                                     (nuids msg-type-number)
                                     (gids :pointer)
                                     (ngids msg-type-number))
  (with-lookup protid file
    (let* ((new-user (make-iouser-mem uids nuids gids ngids))
           (restricted-user (restrict-iouser (get-user protid)
                                             new-user))
           (new-protid (new-protid *translator*
                                   restricted-user
                                   (open-node protid))))
      (setf (mem-ref new-port 'port) (get-right new-protid)
            (mem-ref new-port-type 'msg-type-name) :make-send)
      t)))
      
