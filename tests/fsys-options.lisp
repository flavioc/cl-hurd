
(in-package :translator-test)

(def-test-method fsys-options-test ((test fsys-test))
  (with-testport (p (file-name-lookup +translator-root+))
    (with-testport (cntl (file-getcontrol p))
      (let ((old-options (fsys-get-options cntl)))
        (assert-true (not (has-translator-option-p old-options :cool)))
        (let ((new-options (make-translator-options
                             '((:cool 15)
                               (:hot 40)
                               :fast))))
          (with-cleanup (fsys-set-options cntl :options old-options)
            (assert-true (fsys-set-options cntl
                                           :options new-options))
            (setf new-options (fsys-get-options cntl))
            (assert-true (has-translator-option-p new-options
                                                  :cool))
            (assert-true (has-translator-option-p new-options
                                                  :hot))
            (assert-true (has-translator-option-p new-options
                                                  :fast))
            (assert-equal (get-translator-option new-options
                                                 :cool)
                          15)
            (assert-equal (get-translator-option new-options
                                                 :hot)
                          40)
            (assert-equal (get-translator-option new-options
                                                 :fast)
                          t)
            (assert-true (fsys-set-options cntl :options old-options))))))))

