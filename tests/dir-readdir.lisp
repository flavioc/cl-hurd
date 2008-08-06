
(in-package :translator-test)

(defun %my-readdir (dir &key (entry 0)
                        (nentries -1))
  (loop for item in (dir-readdir dir
                                 :entry entry
                                 :nentries nentries)
        collect (list (dirent-name item)
                      (dirent-type item))))

(def-test-method dir-readdir-test ((test fs-test))
  (with-testport (dir (file-name-lookup +main-dir+
                                        :flags '(:read)))
    (let ((size (length (dir-readdir dir))))
      (assert-equal '(("." :dir)
                      (".." :dir)
                      ("a" :reg)
                      ("b"  :reg)
                      ("c" :reg)
                      ("d" :reg)
                      ("dir1" :dir)
                      ("dir2" :dir)
                      ("dir3" :dir)
                      ("f" :lnk)
                      ("g" :lnk)
                      ("h" :lnk))
                    (%my-readdir dir))
      (assert-equal '(("." :dir)
                      (".." :dir))
                    (%my-readdir dir :nentries 2))
      (assert-equal '(("a" :reg))
                    (%my-readdir dir :nentries 1
                                 :entry 2))
      (assert-equal nil
                    (%my-readdir dir :nentries 0))
      (assert-equal nil
                    (%my-readdir dir :entry 50))
      (assert-equal '(("dir3" :dir)
                      ("f" :lnk)
                      ("g" :lnk)
                      ("h" :lnk))
                    (%my-readdir dir :entry 8)))))


