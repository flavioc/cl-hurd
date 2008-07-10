
(in-package :hurd)

(defbitfield fsys-goaway-flags
  (:nowait #x00000001)
  (:nosync #x00000002)
  (:force  #x00000004)
  (:unlink #x00000008)
  (:recurse #x00000010))

(defun fsys-goaway-flag-is-p (flags flag)
  (if (member flag flags)
    t
    nil))

