
(in-package :hurd-common)

;;
;; This file implements the needed
;; abstractions to deal with the stat struct.
;;

;; POSIX.1b structure for a time value
;; Has seconds and nanoseconds.
;;
(defcstruct timespec-struct
  (sec :unsigned-int)
  (nsec :unsigned-int))

;; Just to be sure..
(assert (= (foreign-type-size 'timespec-struct) 8))

(defconstant +stat-size+ 128 "Size of a stat struct")

(defcstruct (stat-struct :size 128)
  "The stat struct."
	(st-fstype :unsigned-int) ; File system type
	(st-fsid :long-long) ; File system ID
	(st-ino ino-t) ; File number
	(st-gen :unsigned-int) ; To detect reuse of file numbers
	(st-rdev :unsigned-int) ; Device if special file
	(st-mode :unsigned-int) ; File mode
	(st-nlink :unsigned-int) ; Number of links
	(st-uid uid-t) ; Owner
	(st-gid gid-t) ; Owning group
	(st-size :long-long) ; Size in bytes
	(st-atim timespec-struct) ; Time of last access
	(st-mtim timespec-struct) ; Time of last modification
	(st-ctim timespec-struct) ; Time of last status change
	(st-blksize :unsigned-int) ; Optimal size of IO
	(st-blocks :long-long) ; Number of 512-byte blocks allocated
	(st-author uid-t) ; File author
	(st-flags :unsigned-int)) ; User defined flags

(defclass stat (base-mode)
  ((ptr :initform nil
        :initarg :ptr
        :accessor ptr
        :documentation "Pointer to a struct stat."))
  (:documentation "Class for objects containing a pointer to a stat struct."))

(defmethod mode-bits ((stat stat))
  "Returns the mode bits from a stat."
  (foreign-slot-value (ptr stat) 'stat-struct 'st-mode))

(defmethod (setf mode-bits) (new-value (stat stat))
  "Sets the mode bits from a stat."
  (setf (foreign-slot-value (ptr stat) 'stat-struct 'st-mode) new-value))

(defun stat-copy (stat-dest stat-src)
  "Copies to 'stat-dest' all the stat information from 'stat-src'."
  (memcpy (ptr stat-dest) (ptr stat-src) +stat-size+))

(defun %stat-time-get (ptr what)
  "Access from a 'ptr' stat struct the 'sec' field from the timespec field 'what'."
  (let ((ptr (foreign-slot-value ptr 'stat-struct what)))
    (make-time-value :seconds (foreign-slot-value ptr 'timespec-struct 'sec)
                     :microseconds (nanosecs->microsecs (foreign-slot-value ptr 'timespec-struct 'nsec)))))

(defmethod stat-get ((stat stat) what)
  "Gets specific information from a stat object.
'what' can be:
st-atime, st-mtime, st-ctime, st-ev, st-mode, st-fstype,
st-fsid, st-ino, st-gen, st-rdev, st-nlink,
st-uid, st-gid, st-size, st-atim, st-mtim, st-ctim,
st-blksize, st-blocks, st-author, st-flags."
  (with-slots ((ptr ptr)) stat
    (case what
      (st-atime (%stat-time-get ptr 'st-atim))
      (st-mtime (%stat-time-get ptr 'st-mtim))
      (st-ctime (%stat-time-get ptr 'st-ctim))
      ; Get type from the mode bits.
      (st-type (get-type stat))
      ; 'st-dev' is an alias to 'st-fsid'.
      (st-dev (foreign-slot-value ptr 'stat-struct 'st-fsid))
      ; We return a mode object here
      (st-mode (make-mode-clone
                 (foreign-slot-value ptr 'stat-struct 'st-mode)))
      ; With st-rdev, we return a device-id object.
      (st-rdev
        (let ((field (foreign-slot-value ptr 'stat-struct 'st-rdev)))
          (make-instance 'device-id
                         :major (get-major-dev field)
                         :minor (get-minor-dev field))))
      (otherwise
        (foreign-slot-value ptr 'stat-struct what)))))

(defun %stat-time-set (ptr field new-value)
  "From a stat pointer 'ptr' set the timespec field 'field' to 'new-value'."
  (let ((timespec (foreign-slot-value ptr 'stat-struct field))) ; Get the field
    (cond
      ((typep new-value 'time-value) ; Test if this is a kernel time-value
       ; Copy the time-value seconds
       ; and convert the microseconds to nanoseconds.
       (setf (foreign-slot-value timespec 'timespec-struct 'sec)
             (time-value-seconds new-value)
             (foreign-slot-value timespec 'timespec-struct 'nsec)
             (microsecs->nanosecs (time-value-microseconds new-value)))
       t)
      (t
        ; For everything else just copy the value to seconds.
        (setf (foreign-slot-value timespec 'timespec-struct 'sec)
              new-value)
        (setf (foreign-slot-value timespec 'timespec-struct 'nsec) 0)
        t))))

(defmethod stat-set ((stat stat) what new-value)
  "Sets a stat field 'what' to 'new-value'.
'what' can have the same values as 'stat-get'."
  (with-slots ((ptr ptr)) stat
    (case what
      (st-atime (%stat-time-set ptr 'st-atim new-value))
      (st-mtime (%stat-time-set ptr 'st-mtim new-value))
      (st-ctime (%stat-time-set ptr 'st-ctim new-value))
      ; Just an alias to st-fsid
      (st-dev
        (setf (foreign-slot-value ptr 'stat-struct 'st-fsid)
              new-value))
      ; We can use device-id objects here.
      (st-rdev
        (setf (foreign-slot-value ptr 'stat-struct 'st-rdev)
              (if (typep new-value 'device-id)
                 (get-device-integer new-value)
                  new-value))) ; We treat 'new-value' as a simple integer value
      (st-mode
        ; If 'new-value' is a mode object, copy its bits
        ; else it must be the mode bitfield itself.
        (setf (foreign-slot-value ptr 'stat-struct 'st-mode)
              (if (typep new-value 'mode)
                (mode-bits new-value)
                new-value)))
      (otherwise
        (setf (foreign-slot-value ptr 'stat-struct what) new-value)))))

; Use the new method...
(defsetf stat-get stat-set)

(defun make-stat (&optional (extra nil)
                            &key
                            (size 0)
                            (mode nil)
                            (uid nil)
                            (gid nil)
                            (type nil))
  "Create a new stat object. 'extra' can be:
a mode object: we copy it to the mode field.
a stat object: we make a copy of it for the new stat object.

Other arguments:
size: initial size for the size field.
"
  (let* ((mem (foreign-alloc 'stat-struct)) ; Allocate memory for a stat
         (obj (make-instance 'stat :ptr mem))) ; Instantiate new object
    ; Don't leak memory.
    (finalize obj (lambda ()
                    (foreign-free mem)))
    (unless (null extra)
      (case (type-of extra)
        (mode
          ; Copy it to the mode field.
          (setf (stat-get obj 'st-mode)
                (mode-bits extra)))
        (stat
          ; Copy the whole thing.
          (memcpy mem (ptr extra) +stat-size+))))
    ; Optional/Key parameters go here:
    (when (numberp size)
      (setf (stat-get obj 'st-size) size))
    (when mode
      (setf (stat-get obj 'st-mode) mode))
    (when type
      (set-type obj type))
    (when (valid-id-p uid)
      (setf (stat-get obj 'st-uid) uid))
    (when (valid-id-p gid)
      (setf (stat-get obj 'st-gid) gid))
    ; Return the new object
    obj))

(defmethod print-object ((stat stat) stream)
  "Print a stat object."
  (format stream "#<stat: ")
  ; Print the mode object too
  (print-object (stat-get stat 'st-mode) stream)
  (format stream ">"))

(define-foreign-type stat-type ()
  ()
  (:documentation "CFFI type for stat objects.")
  (:actual-type :pointer)
  (:simple-parser stat-t))

(defmethod translate-to-foreign (stat (type stat-type))
  "Translate a stat object to a foreign pointer."
  (ptr stat))

(defmethod translate-from-foreign (value (type stat-type))
  "Translate a stat pointer to a stat object."
  (make-instance 'stat :ptr value))
