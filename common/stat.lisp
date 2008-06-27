
(defcstruct timespec-struct
	    (sec :unsigned-int)
	    (nsec :long))

;; struct stat occupies 128 bytes
(defcstruct (stat-struct :size 128)
	(fstype :int)
	(fsid :unsigned-int)
	(ino :unsigned-int)
	(gen :unsigned-int)
	(rdev :unsigned-int)
	(mode :unsigned-int)
	(nlink :unsigned-int)
	(uid :unsigned-int)
	(gid :unsigned-int)
	(size :unsigned-int)
	(atim timespec-struct)
	(mtim timespec-struct)
	(ctim timespec-struct)
	(blksize :unsigned-int)
	(blocks :unsigned-int)
	(author :unsigned-int)
	(flags :unsigned-int))


(defclass stat (base-mode)
  ((ptr :initform nil
	:initarg :ptr)))

(defmethod mode-bits ((stat stat))
  (stat-get stat 'mode))

(defmethod (setf mode-bits) (new-value (stat stat))
  (stat-set stat 'mode new-value))

(defun make-stat (&optional (mode nil))
  (let* ((mem (foreign-alloc 'stat-struct))
	 (obj (make-instance 'stat :ptr mem)))
    (finalize obj (lambda ()
		    (foreign-free mem)))
    (unless (null mode)
      (setf (stat-get obj 'mode) (mode-bits mode)))
    obj))

(defmethod stat-get ((stat stat) what)
  (with-slots ((ptr ptr)) stat
    (case what
      (atime (foreign-slot-value (foreign-slot-value ptr 'stat-struct 'atim)
				 'timespec-struct 'sec))
      (mtime (foreign-slot-value (foreign-slot-value ptr 'stat-struct 'mtim)
				 'timespec-struct 'sec))
      (ctime (foreign-slot-value (foreign-slot-value ptr 'stat-struct 'ctim)
				 'timespec-struct 'sec))
      (dev (foreign-slot-value ptr 'stat-struct 'fsid))
      (otherwise
	(foreign-slot-value ptr 'stat-struct what)))))

(defmethod stat-set ((stat stat) what new-value)
  (with-slots ((ptr ptr)) stat
    (case what
      (atime
	(let ((timespec (foreign-slot-value ptr 'stat-struct 'atim)))
	  (setf (foreign-slot-value timespec 'timespec-struct 'sec)
		new-value)))
      (mtime
	(let ((timespec (foreign-slot-value ptr 'stat-struct 'mtim)))
	  (setf (foreign-slot-value timespec 'timespec-struct 'sec)
		new-value)))
      (ctime
	(let ((timespec (foreign-slot-value ptr 'stat-struct 'ctim)))
	  (setf (foreign-slot-value timespec 'timespec-struct 'sec)
		new-value)))
      (dev
	(setf (foreign-slot-value ptr 'stat-struct 'fsid) new-value))
      (otherwise
	(setf (foreign-slot-value ptr 'stat-struct what) new-value)))))

(defsetf stat-get stat-set)

(defmethod print-object ((stat stat) stream)
  (format stream "#<stat: ")
  (call-next-method)
  (format stream ">"))

(defvar *s* (make-stat))
