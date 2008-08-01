
(in-package :translator-test)

(def-test-method server-version-test ((test io-test))
  (with-port-deallocate (root +translator-root+)
    (assert-equal (list "test-translator" 1 2 3)
                  (io-server-version root))))
