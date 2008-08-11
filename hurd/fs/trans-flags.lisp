
(in-package :hurd)

;; file-set-translator flags.
(defbitfield fs-trans-flags
  (:force #x00000001)
  (:excl #x00000002)
  (:set #x00000004)
  (:orphan #x00000008))

