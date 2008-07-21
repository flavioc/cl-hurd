
(in-package :hurd-common)

(defbitfield lock-flags
  (:lock-sh 1)
  (:lock-ex 2)
  (:lock-un 8)
  (:lock-nb 4))
