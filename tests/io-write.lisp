
(in-package :translator-test)

(def-test-method io-write-test ((test io-test))
  (let ((file (concatenate-string +main-dir+ "/d")))
    (with-testport (p (file-name-lookup file
                                        :flags '(:read)))
      (multiple-value-bind (ret err)
        (io-write p "a")
        (assert-equal err :invalid-argument)))
    (with-testport (p (file-name-lookup file
                                        :flags '(:creat :read :write)))
      (assert-true (file-set-size p 3))
      (assert-true (io-write p "456"))
      (%my-io-read p "456" :offset 0)
      (assert-true (io-seek p :offset 0
                            :whence :seek-set))
      (assert-true (io-seek p :offset 1))
      (assert-true (io-write p "3"))
      (%my-io-read p "436" :offset 0)
      (assert-true (io-seek p :offset -1 :whence :seek-end))
      (assert-true (io-write p "6789"))
      (%my-io-read p "436789" :offset 0)
      (assert-true (io-write p "5" :offset 1))
      (%my-io-read p "456789" :offset 0)
      (assert-true (file-set-size p 3))
      (%my-io-read p "456" :offset 0))
    (with-testport (p (file-name-lookup file :flags '(:read :write :append)))
      (assert-true (io-write p "789"))
      (%my-io-read p "456789" :offset 0))))

