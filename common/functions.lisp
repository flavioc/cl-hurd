
(defcfun ("strerror" %strerror) :string (code err))

(defun error->string (error-code)
  (%strerror error-code))

(defcfun ("getuid" %getuid) :unsigned-int)

(defun getuid () (%getuid))

(defcfun ("geteuid" %geteuid) :unsigned-int)

(defun geteuid () (%geteuid))

(defcfun ("getgid" %getgid) :unsigned-int)

(defun getgid () (%getgid))

(defcfun ("getegid" %getegid) :unsigned-int)

(defun getegid () (%getegid))
