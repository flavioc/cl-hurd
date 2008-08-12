
(in-package :hurd)

(defcfun ("pid2task" %pid2task)
  task
  (pid pid-t))

(defun pid2task (pid)
  "Return the task control port of process PID."
  (%pid2task pid))

