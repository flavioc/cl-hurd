
(def-io-interface :io-server-version ((io port)
									  (server :pointer)
									  (major-version :pointer)
									  (minor-version :pointer)
									  (edit-version :pointer))
  (when (port-exists io)
	(with-accessors ((name name) (version version)) *translator*
	  (assert (= 3 (length version)))
	  (lisp-string-to-foreign name server (+ 1 (length name)))
	  (setf (mem-ref major-version :int) (first version)
			(mem-ref minor-version :int) (second version)
			(mem-ref edit-version :int) (third version)))
	t))

