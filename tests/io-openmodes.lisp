
(in-package :translator-test)

(def-test-method io-openmodes-test ((test io-test))
  (with-testport (p (file-name-lookup (concatenate-string +translator-root+
                                                                 "/root/a") :flags '(:read)))
    (assert-equal '(:read) (io-get-openmodes p))
    (assert-true (io-set-some-openmodes p '(:nonblock)))
    (assert-equal '(:nonblock :read)
                  (io-get-openmodes p))
    (assert-true (io-set-all-openmodes p '(:append :async :fsync)))
    (assert-equal '(:fsync :async :append :read) (io-get-openmodes p))
    (assert-true (io-clear-some-openmodes p '(:append :fsync)))
    (assert-equal '(:async :read) (io-get-openmodes p))
    (assert-true (io-set-all-openmodes p '()))
    (assert-equal '(:read) (io-get-openmodes p))))

