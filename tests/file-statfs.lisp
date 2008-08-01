
(in-package :translator-test)

(def-test-method file-statfs-test ((test fs-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (assert-true (typep (file-statfs p) 'statfs))))

