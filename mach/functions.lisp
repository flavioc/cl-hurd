
(in-package :mach)

(defun port-valid (p)
  "Checks if port code is a valid port."
  (and (numberp p)
       (> p 0)))

