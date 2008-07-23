
(in-package :hurd-common)

;; Possible arguments to io_select
(defbitfield select-type
  (:read #x00000001)
  (:write #x00000002)
  (:urg #x00000004))

