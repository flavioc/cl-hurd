
(in-package :translator-test)

(def-test-method dir-dir-test ((test fs-test))
  (with-testport (file (file-name-lookup (concatenate-string
                                           +main-dir+
                                           "/a")))
    (multiple-value-bind (ret err)
      (dir-rmdir file "thing")
      (assert-equal ret nil)
      (assert-equal err :not-directory))
    (multiple-value-bind (ret err)
      (dir-mkdir file "thingy")
      (assert-equal ret nil)
      (assert-equal err :not-directory)))
  (with-testport (dir (file-name-lookup +main-dir+))
    (multiple-value-bind (ret err)
      (dir-rmdir dir "mydir")
      (assert-equal ret nil)
      (assert-equal err :no-such-file))
    (multiple-value-bind (ret err)
      (dir-rmdir dir "a")
      (assert-equal ret nil)
      (assert-equal err :not-directory))
    (multiple-value-bind (ret err)
      (dir-rmdir dir "dir1")
      (assert-equal ret nil)
      (assert-equal err :directory-not-empty))
    (assert-true (dir-mkdir dir "mydir"))
    (with-testport (newdir (file-name-lookup
                             (concatenate-string
                               +main-dir+
                               "/mydir")))
      (assert-true (is-dir-p (io-stat newdir))))
    (assert-true (dir-rmdir dir "mydir"))))

