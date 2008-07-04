
(defun %is-minus-one-p (val)
  (or (= val #xffffffff) ;; unsigned-int
	  (= val #xffffffffffffffff))) ;; long-long

