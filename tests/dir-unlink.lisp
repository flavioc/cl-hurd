
(in-package :translator-test)

(def-test-method dir-unlink-test ((test fs-test))
  (with-testport (dir (file-name-lookup +main-dir+))
    (multiple-value-bind (ret err)
      (dir-unlink dir "mylink")
      (declare (ignore ret))
      (assert-equal err :no-such-file))))

