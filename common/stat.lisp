
;; mach time_value struct
;;
(defcstruct time-value-struct
	(seconds :int)
	(microseconds :int))

(define-foreign-type time-value-type ()
					 ()
					 (:actual-type :pointer)
					 (:simple-parser time-value-t))

(defclass time-value ()
  ((ptr :reader ptr
		:initarg :ptr)))

(defmethod translate-from-foreign (value (type time-value-type))
  (if (= -1 (foreign-slot-value value 'time-value-struct 'microseconds))
	nil
	(make-instance 'time-value :ptr value)))

(defmethod translate-to-foreign (value (type time-value-type))
  (ptr value))

(defcstruct timespec-struct
	    (sec :unsigned-int)
	    (nsec :unsigned-int))

(assert (= (foreign-type-size 'timespec-struct) 8))

(defconstant +stat-size+ 128)

;; struct stat occupies 128 bytes
(defcstruct (stat-struct :size 128)
	(fstype :unsigned-int)
	(fsid :long-long)
	(ino ino-t)
	(gen :unsigned-int)
	(rdev :unsigned-int)
	(mode :unsigned-int)
	(nlink :unsigned-int)
	(uid uid-t)
	(gid gid-t)
	(size :long-long)
	(atim timespec-struct)
	(mtim timespec-struct)
	(ctim timespec-struct)
	(blksize :unsigned-int)
	(blocks :long-long)
	(author uid-t)
	(flags :unsigned-int))

(defclass stat (base-mode)
  ((ptr :initform nil
	:initarg :ptr
	:accessor ptr)))

(defmethod mode-bits ((stat stat))
  (foreign-slot-value (ptr stat) 'stat-struct 'mode))

(defmethod (setf mode-bits) (new-value (stat stat))
  (setf (foreign-slot-value (ptr stat) 'stat-struct 'mode) new-value))

(defun make-stat (&optional (extra nil))
  (let* ((mem (foreign-alloc 'stat-struct))
	 (obj (make-instance 'stat :ptr mem)))
    (finalize obj (lambda ()
		    (foreign-free mem)))
    (unless (null extra)
	  (case (type-of extra)
		(mode (setf (stat-get obj 'mode)
					(mode-bits mode)))
		(stat (memcpy mem (ptr extra) +stat-size+))
		(t
		  nil)))
    obj))

(defun copy-stat-struct (stat-dest stat-src)
  (memcpy (ptr stat-dest) (ptr stat-src) +stat-size+))

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
	  (mode (make-mode-clone
			  (foreign-slot-value ptr 'stat-struct 'mode)))
      (otherwise
	(foreign-slot-value ptr 'stat-struct what)))))

(defun set-stat-time (ptr field new-value)
  (let ((timespec (foreign-slot-value ptr 'stat-struct field)))
	(cond
	  ((typep new-value 'time-value)
	   (setf (foreign-slot-value timespec 'timespec-struct 'sec)
				 (foreign-slot-value (ptr new-value) 'time-value-struct 'seconds)
			 (foreign-slot-value timespec 'timespec-struct 'nsec)
				 (* 1000
					(foreign-slot-value (ptr new-value) 'time-value-struct 'microseconds))))
	  (t
		(setf (foreign-slot-value timespec 'timespec-struct 'sec)
			  new-value)))))

(defmethod stat-set ((stat stat) what new-value)
  (with-slots ((ptr ptr)) stat
	(case what
	  (atime
		(set-stat-time ptr 'atim new-value))
	  (mtime
		(set-stat-time ptr 'mtim new-value))
	  (ctime
		(set-stat-time ptr 'ctim new-value))
	  (dev
		(setf (foreign-slot-value ptr 'stat-struct 'fsid) new-value))
	  (mode
		(setf (foreign-slot-value ptr 'stat-struct 'mode)
			  (if (typep new-value 'mode)
				(mode-bits new-value)
				new-value)))
	  (otherwise
		(setf (foreign-slot-value ptr 'stat-struct what) new-value)))))

(defsetf stat-get stat-set)

(defmethod print-object ((stat stat) stream)
  (format stream "#<stat: ")
  (call-next-method)
  (format stream ">"))

(define-foreign-type stat-type ()
					 ()
					 (:actual-type :pointer)
					 (:simple-parser stat-t))

(defmethod translate-to-foreign (stat (type stat-type))
  (ptr stat))

(defmethod translate-from-foreign (value (type stat-type))
  (make-instance 'stat :ptr value))
