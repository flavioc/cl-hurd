
(in-package :translator-test)

(defun %my-lookup (under name &key flags retry)
  (multiple-value-bind (retry-type retry-name port)
    (dir-lookup under
                name
                :flags flags)
    (declare (ignore retry-type))
    (cond
      ((null port)
       retry-name)
      (t
        (port-deallocate port)
        (when retry
          (assert-equal retry retry-name))
        t))))

(def-test-method dir-lookup-test ((test fs-test))
  (with-testport (root (file-name-lookup +translator-root+))
    ; Lookup inexistent file
    (assert-equal :no-such-file (%my-lookup root
                                            "abc"))
    (assert-equal :no-such-file (%my-lookup root
                                            "root/./abc/../a"))
    ; Try to create an existing file
    (assert-equal :file-exists (%my-lookup root
                                           "root"
                                           :flags '(:creat :excl)))
    ; Try to open in a non existent directory
    (assert-equal :not-directory (%my-lookup root
                                             "root/a/f"))
    ; Try to lookup a circular link
    (assert-equal :too-many-links (%my-lookup root
                                              "root/g"))
    ; Try a normal lookup
    (assert-true (%my-lookup root "root"))
    (assert-true (%my-lookup root "root/a"))
    (assert-true (%my-lookup root "root/f"))
    (assert-true (%my-lookup root "root/f" :flags '(:nolink :notrans)))
    (assert-true (%my-lookup root "root/dir1/a"))
    (assert-true (%my-lookup root "root/dir1/././b" :flags '(:read)))
    (assert-true (%my-lookup root "root/dir1/././../dir2/././../../root/f"))
    (assert-true (%my-lookup root ".."))
    (assert-true (%my-lookup root "../.." :retry ".."))
    (assert-true (%my-lookup root "/usr" :retry "/usr"))
    (assert-true (%my-lookup root "root/h" :retry "/usr"))
    (assert-true (%my-lookup root "root/h/bin" :retry "/usr/bin"))))

