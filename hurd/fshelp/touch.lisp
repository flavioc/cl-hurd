
(in-package :hurd)

;; Touch flags that can be used in touch.
(defbitfield touch-flags
  (:atime #x1) ; Access time
  (:mtime #x2) ; Modification time
  (:ctime #x4)) ; Status change time

(defcfun ("fshelp_touch" %fshelp-touch)
  :void
  (stat :pointer)
  (what touch-flags)
  (maptime :pointer))

(defmethod touch ((stat stat) what &optional (maptime (maptime-map)))
  "Touch 'stat' fields 'what' (combination of touch-flags) with time 'maptime'."
  (%fshelp-touch (ptr stat)
                 what
                 maptime))
