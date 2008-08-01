
(in-package :translator-test)

(def-test-method server-version-test ((test io-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (assert-equal (list "test-translator" 1 2 3)
                  (io-server-version p))))
