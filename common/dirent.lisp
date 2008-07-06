
(in-package :hurd-common)

;;
;; File types for the 'd_type' field
;; from struct dirent (bits/dirent.h).
;; This enum is found at dirent.h.
;;
(defcenum dirent-type
  (:unknown 0)
  (:fifo 1)
  (:chr 2)
  (:dir 4)
  (:blk 6)
  (:reg 8)
  (:lnk 10)
  (:sock 12)
  (:wht 14))

