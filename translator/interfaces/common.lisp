
(in-package :hurd-translator)

(defun %is-minus-one-p (val)
  "Checks if 'val' has the -1 value."
  (or (= val #xffffffff) ;; unsigned-int
	  (= val #xffffffffffffffff))) ;; long-long

