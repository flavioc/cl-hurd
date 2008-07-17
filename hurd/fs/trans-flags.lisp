
(in-package :hurd)

(defbitfield fs-trans-flags
  (:force #x00000001)
  (:excl #x00000002)
  (:set #x00000004)
  (:orphan #x00000008))
