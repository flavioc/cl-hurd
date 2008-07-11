
(in-package :hurd-common)

;; Possible arguments to lseek/fseek/io_seek.
(defcenum seek-type
  (:seek-set 0)
  (:seek-cur 1)
  (:seek-end 2))
