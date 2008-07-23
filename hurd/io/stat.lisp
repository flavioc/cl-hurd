
(in-package :hurd)

(defcfun ("io_stat" %io-stat)
  err
  (node port)
  (stat stat-t))

(defun io-stat (port)
  "Returns a stat object from a IO port."
  (let ((stat (make-stat)))
    (select-error (%io-stat port stat) stat)))
