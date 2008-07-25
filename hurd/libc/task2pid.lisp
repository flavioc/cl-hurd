
(in-package :hurd)

(defcfun ("task2pid" %task2pid)
  pid-t
  (task task))

(defun task2pid (task)
  "Converts a task into a process id."
  (declare (type fixnum task))
  (let ((err (%task2pid task)))
    (cond
      ((= -1 err) nil)
      (t err))))
