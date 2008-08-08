
(in-package :hurd-common)

(defcfun ("memcmp" %memcmp)
  :int
  (p1 :pointer)
  (p2 :pointer)
  (size size-t))

(defun memcmp (p1 p2 size)
  "Compares 'size' bytes of pointer 'p1' to pointer 'p2'.
Returns T in case they are the same, NIL otherwise."
  (= 0 (%memcmp p1 p2 size)))

