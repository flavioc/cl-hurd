
(define-helper-library portset-demuxer)

(defcfun ("set_demuxer" %set-demuxer)
	 :void
	 (fun :pointer))

(defcfun ("portset_demuxer" %portset-demuxer)
	 :int
	 (in :pointer)
	 (out :pointer))

(defcallback portset-demuxer
	     :int
	     ((in :pointer)
	     (out :pointer))
  (%portset-demuxer in out))

(defmacro set-demuxer (fun)
  (with-gensyms (callback-name)
     `(progn
	(defcallback ,callback-name
		     :boolean
		     ((port port)
		      (in :pointer)
		      (out :pointer))
		     (funcall ,fun port in out))
	(%set-demuxer (callback ,callback-name)))))
