
(in-package :hurd)

(defcfun ("geteuids" %geteuids)
  :int
  (n :int)
  (uids :pointer))

(defconstant +default-geteuids-size+ 20)

(defun geteuids ()
  "Get the effective UID set."
  (with-foreign-pointer (ptr (* +uid-t-size+
                                +default-geteuids-size+))
    (let ((total (%geteuids +default-geteuids-size+ ptr)))
      (loop for i from 0 below total
            collect (mem-aref ptr 'uid-t i)))))

