
(in-package :translator-test)

(def-test-method dir-link-test ((test fs-test))
  (with-testport (dir (file-name-lookup +main-dir+))
    (with-testport (file (file-name-lookup (concatenate-string +main-dir+
                                                               "/a")))
      (assert-true (dir-link dir :file file :name "mylink"))
      (with-testport (test (file-name-lookup (concatenate-string +main-dir+
                                                                 "/mylink"))))
      (assert-true (dir-unlink dir "mylink"))
      (multiple-value-bind (ret err)
        (dir-link dir :file file :name "a" :excl t)
        (declare (ignore ret))
        (assert-equal :file-exists err))
      (with-testport (file2 (file-name-lookup "/usr"))
        (multiple-value-bind (ret err)
          (dir-link dir :file file2 :name "abc" :excl t)
          (declare (ignore ret))
          (assert-equal :invalid-cross-device-link err))))))

