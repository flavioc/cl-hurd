
(in-package :hurd)

(defcfun ("seteuids" %seteuids)
  :int
  (n :int)
  (ptr :pointer))

(defun seteuids (uid-list)
  "Set the effective UID set."
  (declare (type cons uid-list))
  (unless (and (listp uid-list)
               (not (null uid-list)))
    (return-from seteuids nil))
  (let ((total (length uid-list)))
    (with-foreign-pointer (ptr (* +uid-t-size+
                                  total))
      (loop for i from 0 below total
            for uid in uid-list
            do (setf (mem-aref ptr 'uid-t i) uid))
      (let ((err (%seteuids total ptr)))
        (cond
          ((= err -1) nil)
          (t err))))))
