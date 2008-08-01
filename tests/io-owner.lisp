
(in-package :translator-test)

(def-test-method io-owner-test ((test io-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (assert-equal 0 (io-get-owner p))
    (assert-true (io-mod-owner p 101))
    (assert-equal 101 (io-get-owner p))
    (assert-true (io-mod-owner p 0))))
