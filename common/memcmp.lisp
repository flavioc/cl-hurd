
(in-package :hurd-common)

(defcfun ("memcmp" %memcmp)
  :int
  (p1 :pointer)
  (p2 :pointer)
  (size size-t))

(defun memcmp (p1 p2 size)
  (= 0 (%memcmp p1 p2 size)))
