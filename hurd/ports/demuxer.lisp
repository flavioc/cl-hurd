
(in-package :hurd)

;; Defines a new internal demuxer port the portset-demuxer.
(defcfun ("set_demuxer" %set-demuxer)
  :void
  (fun :pointer))

(defcfun ("portset_demuxer" %portset-demuxer)
  :int
  (in :pointer)
  (out :pointer))

(defmacro set-demuxer (fun)
  "Defines fun as the internal demuxer for portset-demuxer."
  (with-gensyms (callback-name)
    `(progn
       (defcallback ,callback-name :boolean
                    ((port port)
                     (in :pointer)
                     (out :pointer))
         (funcall ,fun port in out))
       (%set-demuxer (callback ,callback-name)))))

