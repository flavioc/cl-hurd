
(in-package :hurd)

(defun %idptr-to-list (ptr size)
  "Transforms a foreign array 'ptr' with 'size' elements into a list."
  (loop for i from 0 below size
        collect (mem-aref ptr :unsigned-int i)))

(defun %free-idptr (ptr size)
  "Free's a foreign array with 'size' elements."
  (let ((ptr ptr))
    (loop for i from 0 below size
          do (progn
               (foreign-free ptr)
               (incf-pointer ptr)))))

