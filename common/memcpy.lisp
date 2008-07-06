
(in-package :hurd-common)

(defcfun ("memcpy" %memcpy) :void
  (dest :pointer)
  (src :pointer)
  (size :unsigned-int))

(defun memcpy (dest src size)
  "Copies to 'dest' from 'src', 'size' bytes of memory."
  (%memcpy dest src size))

