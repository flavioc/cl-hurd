
(in-package :hurd-common)

;; This file describes all the lock flags
;; associated with file locking.

(defbitfield lock-flags
  (:lock-sh 1)
  (:lock-ex 2)
  (:lock-un 8)
  (:lock-nb 4))

