
(defcfun ("strerror" %strerror) :string
		 (code err))

(defun error->string (error-code)
  (%strerror error-code))
