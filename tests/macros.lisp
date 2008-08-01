
(in-package :translator-test)

(defmacro with-testport ((name init) &body body)
  `(let ((,name ,init))
     (with-cleanup (port-deallocate ,name)
       (assert-true (port-valid-p ,name))
       ,@body)))

