
(in-package :hurd-common)

(defun exit (&optional (status 0))
  "Exit Lisp with status 'status'."
  #+clisp (ext:exit status)
  #+sbcl (sb-ext:quit :unix-status status)
  #-(or sbcl clisp) (error "(exit) not implemented in your lisp implementation"))
