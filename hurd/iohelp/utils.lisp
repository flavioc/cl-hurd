
(defun idptr-to-list (ptr size)
  (loop for i from 0 below size
	collect (mem-aref ptr :unsigned-int i)))

(defun free-idptr (ptr size)
  (let ((ptr ptr))
    (loop for i from 0 below size
	  do (progn
	       (foreign-free ptr)
	       (incf-pointer ptr)))))
