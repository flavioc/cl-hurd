
(in-package :hurd)

;;
;; File for the foreign retry_type enum.
;; Found at hurd/hurd_types.h
;;

(defcenum retry-type
  (:retry-normal 1)
  (:retry-reauth 2)
  (:retry-magical 3))

