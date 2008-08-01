
(in-package :translator-test)

(def-test-method pathconf-test ((test io-test))
  (with-testport (root (file-name-lookup +translator-root+))
    (assert-equal -1 (io-pathconf root :link-max))
    (assert-equal -1 (io-pathconf root :max-input))
    (assert-equal -1 (io-pathconf root :pipe-buf))
    (assert-equal -1 (io-pathconf root :vdisable))
    (assert-equal -1 (io-pathconf root :sock-maxbuf))
    (assert-equal 1024 (io-pathconf root :name-max))
    (assert-equal 1 (io-pathconf root :chown-restricted))
    (assert-equal 1 (io-pathconf root :no-trunc))
    (assert-equal 0 (io-pathconf root :prio-io))
    (assert-equal 0 (io-pathconf root :sync-io))
    (assert-equal 0 (io-pathconf root :async-io))
    (assert-equal 32 (io-pathconf root :filesizebits))))

