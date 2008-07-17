
(in-package :hurd)

;;
;; Exec flags for file-exec callback.
;;

(defbitfield exec-flags
  (:newtask #x00000001)
  (:secure #x00000002)
  (:defaults #x00000004)
  (:sigtrap #x00000008)
  (:stack-args #x00000010))

