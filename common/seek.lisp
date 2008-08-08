
(in-package :hurd-common)

(defcenum seek-type
  "Possible arguments to lseek/fseek/io-seek."
  (:seek-set 0)
  (:seek-cur 1)
  (:seek-end 2))

