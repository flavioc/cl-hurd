
(in-package :translator-test)

(defun %my-io-read (file result &key (offset +minus-one-ll+)
                         (amount 2048))
  (assert-equal result
                (octets-to-string
                  (io-read file
                           :offset offset
                           :amount amount))))

(def-test-method io-read-test ((test io-test))
  (let ((file (concatenate-string +main-dir+ "/a")))
    (with-testport (p (file-name-lookup file))
      (multiple-value-bind (ret err)
        (io-read p)
        (assert-equal ret nil)
        (assert-equal err :bad-fd)))
    (with-testport (p (file-name-lookup file :flags '(:read)))
      (let* ((stat (io-stat p))
             (size (stat-get stat 'st-size)))
        (assert-equal size (io-readable p))
        ; Try to read everything.
        (%my-io-read p "abcdefghijklmnopqrstuvwxyz")
        (assert-true (io-seek p :offset 0
                              :whence :seek-set))
        ; Read byte by byte
        (loop for i from 0 to size
              do (progn
                   (assert-equal (- size i)
                                 (io-readable p))
                   (if (= i size)
                     (%my-io-read p "" :amount 1)
                     (%my-io-read p (octets-to-string `(,(+ 97 i)))
                                  :amount 1))))
        ; Now without seeking
        (loop for i from 0 below size
              do (%my-io-read p (octets-to-string `(,(+ 97 i)))
                              :offset i
                              :amount 1))
      ; Read outside
      (%my-io-read p ""
                   :offset 2048
                   :amount 20)
      ; Read last byte
      (%my-io-read p "z"
                   :offset (1- size)
                   :amount 2048)
      ; Read in the middle
      (%my-io-read p "fgh"
                   :offset 5
                   :amount 3)
      ; Read sizeable pieces
      (assert-true (io-seek p :offset 0 :whence :seek-set))
      (%my-io-read p "abcdef"
                   :amount 6)
      (%my-io-read p "ghijkl"
                   :amount 6)
      (assert-true (io-seek p :offset 3))
      (%my-io-read p "pqrstu"
                   :amount 6)
      (%my-io-read p "vwxyz"
                   :amount 6)
      (%my-io-read p ""
                   :amount 6)))))


