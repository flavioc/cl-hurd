
(in-package :hurd)

(defcfun ("dir_readdir" %dir-readdir)
  err
  (dir port)
  (data :pointer)
  (data-cnt :pointer)
  (entry :int)
  (nentries :int)
  (bufsiz vm-size)
  (amount :pointer))

(defconstant +dir-readdir-start-size+ 2048
             "Initial block size for dirent data as libc's readdir64.c")

(defun %get-entries (ptr total)
  (loop for i from 0 below total
        collect (let ((entry (read-dirent ptr)))
                  (incf-pointer ptr (size entry))
                  entry)))

(defun dir-readdir (dir &optional (entry 0) (nentries -1) (bufsiz 0))
  (declare (type fixnum dir entry nentries bufsiz))
  (when (or (zerop nentries)
            (< nentries -1))
    (return-from dir-readdir nil))
  (with-foreign-pointer (data +dir-readdir-start-size+)
    (with-foreign-pointer (data-cnt (foreign-type-size 'msg-type-number))
      (setf (mem-ref data-cnt 'msg-type-number) +dir-readdir-start-size+)
      (with-foreign-pointer (dataptr (foreign-type-size :pointer))
        (setf (mem-ref dataptr :pointer) data)
        (with-foreign-pointer (amount (foreign-type-size :int))
          (select-error
			(%dir-readdir dir data-ptr data-cnt
						  entry nentries bufsiz amount)
			(let ((ptr (mem-ref dataptr :pointer))
				  (total (mem-ref data-cnt 'msg-type-number)))
			  (with-cleanup (unless (pointer-eq ptr data)
							  (munmap ptr total))
			    (%get-entries ptr (mem-ref amount :int))))))))))

