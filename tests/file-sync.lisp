
(in-package :translator-test)

(def-test-method file-sync-test ((test fs-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (assert-true (file-sync p))
    (assert-true (file-syncfs p))))

