
(in-package :translator-test)

(def-test-method file-chauthor-test ((test fs-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (let ((stat (io-stat p)))
      (assert-equal 0 (stat-get stat 'st-author))
      (assert-true (file-chauthor p 100))
      (setf stat (io-stat p))
      (assert-equal 100 (stat-get stat 'st-author))
      (assert-true (file-chauthor p 0))
      (setf stat (io-stat p))
      (assert-equal 0 (stat-get stat 'st-author)))))
