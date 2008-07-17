
(in-package :hurd-translator)

(def-fs-interface :file-get-translator-cntl ((file port)
                                             (cntl port-pointer)
                                             (cntl-type :pointer))
  (with-lookup protid file
    (block file-get-translator-cntl
           (let ((node (get-node protid))
                 (user (get-user protid)))
             (unless (is-owner-p node user)
               (return-from file-get-translator-cntl :permission-denied))
             (unless (box-translated-p (box node))
               (return-from file-get-translator-cntl :no-such-device-address))
             (setf (mem-ref cntl 'port) (box-fetch-control (box node))
                   (mem-ref cntl-type 'msg-type-name) :move-send)
             t))))
