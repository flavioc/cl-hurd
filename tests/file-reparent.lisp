
(in-package :translator-test)

(def-test-method file-reparent-test ((test fs-test))
  (let ((dir1-name (concatenate-string +translator-root+
                                       "/root/dir1"))
        (root-name (concatenate-string +translator-root+
                                       "/root")))
    (with-testport (p-root (file-name-lookup +translator-root+))
      (with-testport (p-dir1 (file-name-lookup dir1-name))
        (with-testport (p-root-dir (file-name-lookup root-name))
          (let ((root-stat (io-stat p-root)))
            (with-testport (p-reparent (file-reparent p-dir1 p-root))
              (with-testport (mustberoot (file-name-lookup ".."
                                                           :under p-reparent))
                (assert-true (stat-eq root-stat
                                      (io-stat mustberoot)))
                (assert-true (not (stat-eq (io-stat p-root-dir)
                                           (io-stat mustberoot))))
                (with-testport (oldparent (file-name-lookup ".."
                                                            :under p-dir1))
                  (assert-true (not (stat-eq (io-stat oldparent)
                                             (io-stat mustberoot)))))))))))))
