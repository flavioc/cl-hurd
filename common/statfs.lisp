
(in-package :hurd-common)

(defctype blkcnt :long-long "__fsblkcnt64_t type.")
(defctype filcnt :long-long "__fsfilcnt64_t type.")

(defbitfield statfs-flags
  (:rdonly 1)
  (:nosuid 2)
  (:noexec 8)
  (:synchronous 16))

(defconstant +statfs-size+ 88)

(defcstruct (statfs-struct :size 88)
  (type fs-type) ; Type of filesystem.
  (bsize :unsigned-int) ; Optimal transfer block.

  (blocks blkcnt) ; Total data blocks.
  (bfree blkcnt) ; Total blocks free.
  (bavail blkcnt) ; Total blocks available.
  (files blkcnt) ; Total file nodes.
  (ffree blkcnt) ; Free file nodes.

  (fsid :long-long) ; File system id.
  (namelen :unsigned-int) ; Maximum file name length.

  (favail filcnt) ; Total number of free file nodes (inodes) available to non-privileged processes.

  (frsize :unsigned-int) ; Fundamental file system block size (fragment size).
  (flags statfs-flags))

(defclass statfs ()
  ((ptr :initform nil
        :initarg :ptr
        :accessor ptr
        :documentation "Pointer to a statfs-struct."))
  (:documentation "Allocates a statfs-struct pointer."))

(defun make-statfs (&optional ptr)
  (let ((ptr-null-p (null ptr)))
    (when ptr-null-p
      (setf ptr (foreign-alloc 'statfs-struct)))
    (let ((obj (make-instance 'statfs :ptr ptr)))
      (when ptr-null-p
        (tg:finalize obj (lambda () (foreign-free ptr))))
      obj)))

(defmethod statfs-clean ((stat statfs))
  "Zeroes the statfs memory."
  (bzero (ptr stat) (foreign-type-size 'statfs-struct)))

(defmethod statfs-get ((stat statfs) what)
  (foreign-slot-value (ptr stat) 'statfs-struct what))

(defmethod statfs-set ((stat statfs) what new-value)
  (setf (foreign-slot-value (ptr stat) 'statfs-struct what) new-value))

(defsetf statfs-get statfs-set)

(defun statfs-copy (statfs-dest statfs-src)
  "Copies to 'statfs-dest' all the statfs information from 'statfs-src'."
  (memcpy (ptr statfs-dest) (ptr statfs-src) +statfs-size+))

(define-foreign-type statfs-type ()
  ()
  (:documentation "CFFI type for statfs objects.")
  (:actual-type :pointer)
  (:simple-parser statfs-t))

(defmethod translate-to-foreign (value (type statfs-type))
  "Translates a statfs object to a statfs-struct pointer."
  (ptr value))

(defmethod translate-from-foreign (value (type statfs-type))
  "Translates a statfs-struct pointer to a statfs object."
  (make-statfs value))

