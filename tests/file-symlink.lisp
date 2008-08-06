
(in-package :translator-test)

(def-test-method file-symlink-test ((test fs-test))
  "This does the same thing as symlink.c from glibc."
  (with-testport (p (file-name-lookup +main-dir+))
    (with-testport (file (dir-mkfile p
                                     :flags '(:write)
                                     :mode (make-mode :perms '((:owner :read :write :exec)
                                                               (:group :read :write :exec)
                                                               (:others :read :write :exec)))))
       (assert-true (file-set-translator file
                                         :path (list +hurd-symlink+ "/usr")))
       (assert-true (dir-link p
                              :file file
                              :name "mylink"
                              :excl t))
       (assert-equal (file-get-translator file)
                     (list +hurd-symlink+ "/usr"))
       (assert-true (dir-unlink p "mylink")))))
