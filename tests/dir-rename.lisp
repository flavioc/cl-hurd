
(in-package :translator-test)

(defun %my-dir-rename (dir newdir oldname newname
                           ret err)
  (multiple-value-bind (ret0 err0)
    (dir-rename dir newdir
                :oldname oldname
                :newname newname
                :excl t)
    (assert-equal ret0 ret)
    (assert-equal err0 err)))

(def-test-method dir-rename-test ((test fs-test))
  (with-testport (dir (file-name-lookup +main-dir+))
    ; Trigger invalid cross device link
    (with-testport (newdir (file-name-lookup "/usr"))
      (%my-dir-rename dir newdir "a" "a"
                      nil :invalid-cross-device-link))
    ; Trigger no such file
    (with-testport (newdir (file-name-lookup +main-dir+))
      (%my-dir-rename dir newdir "abcd" "x" nil :no-such-file))
    ; Trigger a file exists
    (with-testport (newdir (file-name-lookup +main-dir+))
      (%my-dir-rename dir newdir "a" "a" nil :file-exists))
    ; Now some successful rename's..
    (with-testport (newdir (file-name-lookup +translator-root+))
      (let (old-stat new-stat)
        (with-testport (file (file-name-lookup (concatenate-string
                                                 +main-dir+
                                                 "/a")))
          (setf old-stat (io-stat file)))
        (assert-true (dir-rename dir newdir :oldname "a" :newname "a"))
        (with-testport (file (file-name-lookup (concatenate-string
                                                 +translator-root+
                                                 "/a")))
          (setf new-stat (io-stat file)))
        (assert-true (stat-eq old-stat new-stat)))
      (assert-true (dir-rename newdir newdir
                               :oldname "a"
                               :newname "thingy"))
      (assert-true (dir-rename newdir dir
                               :oldname "thingy"
                               :newname "a")))))

