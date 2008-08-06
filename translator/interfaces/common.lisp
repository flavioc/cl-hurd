
(in-package :hurd-translator)

(defun %is-minus-one-p (val)
  "Checks if 'val' has the -1 value."
  (or (= val +minus-one+) ;; unsigned-int
	  (= val +minus-one-ll+))) ;; long-long

