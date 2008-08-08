
(in-package :hurd-common)

(defbitfield select-type
  "Possible arguments to io-select."
  (:read #x00000001)
  (:write #x00000002)
  (:urg #x00000004))

