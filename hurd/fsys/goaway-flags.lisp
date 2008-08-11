
(in-package :hurd)

;; Flags that can be passed to fsys-goaway.
(defbitfield fsys-goaway-flags
  (:nowait #x00000001)
  (:nosync #x00000002)
  (:force  #x00000004)
  (:unlink #x00000008)
  (:recurse #x00000010))
