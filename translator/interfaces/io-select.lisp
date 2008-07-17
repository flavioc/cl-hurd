
(in-package :hurd-translator)

;; XXX: what does :urg mean ?
;;
(def-io-interface :io-select ((file port)
                              (type :pointer))
  (with-lookup protid file
    (block io-select
           (let ((node (get-node protid))
                 (user (get-user protid))
                 (open (open-node protid))
                 (select-flags (mem-ref type 'select-type)))
             (when (and (not (flag-is-p (flags open) :read))
                        (flag-is-p select-flags :read))
               (return-from io-select :invalid-argument))
             (when (and (not (flag-is-p (flags open) :write))
                        (flag-is-p select-flags :write))
               (return-from io-select :invalid-argument))
             (let ((ret-flags))
               (when (flag-is-p select-flags :read)
                 (if (block-read *translator*
                                 node
                                 user)
                   (push :read ret-flags)))
               (when (flag-is-p select-flags :write)
                 (if (block-write *translator*
                                  node
                                  user)
                   (push :write ret-flags)))
               (setf (mem-ref type 'select-type) ret-flags)
               t)))))

