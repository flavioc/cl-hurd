
(in-package :translator-test)

(def-test-method file-getlinknode-test ((test fs-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (assert-true (null (file-getlinknode p)))
    (with-testport (p1 (file-name-lookup (concatenate-string +translator-root+
                                                             "/root/a")))
      (with-testport (p2 (file-getlinknode p1))
        (assert-true (stat-eq (io-stat p1)
                              (io-stat p2)))))))
