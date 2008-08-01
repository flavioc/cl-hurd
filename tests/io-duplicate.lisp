
(in-package :translator-test)

(def-test-method io-duplicate-test ((test io-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (with-testport (p2 (io-duplicate p))
      (assert-true (stat-eq (io-stat p)
                            (io-stat p2))))))
