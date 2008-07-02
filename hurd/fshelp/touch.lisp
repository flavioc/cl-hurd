
(defbitfield touch-flags
	     (:atime #x1)
	     (:mtime #x2)
	     (:ctime #x4))

(defcfun ("fshelp_touch" %fshelp-touch)
	 :void
	 (stat :pointer)
	 (what touch-flags)
	 (maptime :pointer))

(defmethod touch ((stat stat) what &optional (maptime (maptime-map)))
  (%fshelp-touch (ptr stat)
		 what
		 maptime))
