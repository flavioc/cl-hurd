
(in-package :hurd)

(defcfun ("pid2task" pid2task)
  task
  (pid pid-t))
