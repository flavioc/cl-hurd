
(in-package :hurd-translator)

(def-io-interface :io-stat ((io port)
                            (stat-info stat-t))
  (with-lookup protid io
    (stat-copy stat-info (get-stat protid))
    (set-root stat-info
              (or (eq (get-node protid) (get-shadow-root protid))
                  (eq (get-node protid) (root *translator*))))
    t))

