
(in-package :translator-test)

(def-test-method file-utimes-test ((test fs-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (let ((stat (io-stat p))
          (tval1 (make-time-value
                  :seconds 50000
                  :microseconds 1))
          (tval2 (make-time-value
                   :seconds 20000
                   :microseconds 5)))
      (assert-true (file-utimes p :atime tval1 :mtime tval2))
      (let* ((new-stat (io-stat p))
             (a-tval (stat-get new-stat 'st-atime))
             (m-tval (stat-get new-stat 'st-mtime)))
        (assert-equal (time-value-seconds a-tval)
                      (time-value-seconds tval1))
        (assert-equal (time-value-microseconds a-tval)
                      (time-value-microseconds tval1))
        (assert-equal (time-value-seconds m-tval)
                      (time-value-seconds tval2))
        (assert-equal (time-value-microseconds m-tval)
                      (time-value-microseconds tval2))
        (assert-true (file-utimes p
                                  :atime (stat-get stat 'st-atime)
                                  :mtime (stat-get stat 'st-mtime))))
      (let ((new-stat (io-stat p)))
        (assert-true (time-value-eq (stat-get new-stat 'st-atime)
                                    (stat-get stat 'st-atime)))
        (assert-true (time-value-eq (stat-get new-stat 'st-mtime)
                                    (stat-get stat 'st-mtime))))
      (assert-true (file-utimes p))
      (let ((new-stat (io-stat p)))
        (assert-true (time-value-newer-p (stat-get new-stat 'st-atime)
                                         (stat-get stat 'st-atime)))
        (assert-true (time-value-newer-p (stat-get new-stat 'st-mtime)
                                         (stat-get stat 'st-atime))))
      (assert-true (file-utimes p
                                :atime (stat-get stat 'st-atime)
                                :mtime (stat-get stat 'st-mtime))))))

