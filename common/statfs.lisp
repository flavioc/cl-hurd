
(in-package :hurd-common)

;;
;; This file implements the statfs abstraction
;; to deal with statfs pointers.
;;

(defctype blkcnt :long-long "__fsblkcnt64_t type.")
(defctype filcnt :long-long "__fsfilcnt64_t type.")

(defbitfield statfs-flags
  "Different flags for the f-flags statfs field."
  (:rdonly 1)
  (:nosuid 2)
  (:noexec 8)
  (:synchronous 16))

(defconstant +statfs-size+ 88)

(defcstruct (statfs-struct :size 88)
  (f-type fs-type) ; Type of filesystem.
  (f-bsize :unsigned-int) ; Optimal transfer block.

  (f-blocks blkcnt) ; Total data blocks.
  (f-bfree blkcnt) ; Total blocks free.
  (f-bavail blkcnt) ; Total blocks available.
  (f-files blkcnt) ; Total file nodes.
  (f-ffree blkcnt) ; Free file nodes.

  (f-fsid :long-long) ; File system id.
  (f-namelen :unsigned-int) ; Maximum file name length.

  (f-favail filcnt) ; Total number of free file nodes (inodes) available to non-privileged processes.

  (f-frsize :unsigned-int) ; Fundamental file system block size (fragment size).
  (f-flags statfs-flags))

(defclass statfs ()
  ((ptr :initform nil
        :initarg :ptr
        :accessor ptr
        :documentation "Pointer to a statfs-struct."))
  (:documentation "Allocates a statfs-struct pointer."))

(defun make-statfs (&optional ptr)
  "Create a new statfs object, allocating a new pointer in case 'ptr' is NIL."
  (let ((ptr-null-p (null ptr)))
    (when ptr-null-p
      (setf ptr (foreign-alloc 'statfs-struct)))
    (let ((obj (make-instance 'statfs :ptr ptr)))
      (when ptr-null-p
        ; Free resources when we don't need them.
        (tg:finalize obj (lambda () (foreign-free ptr))))
      obj)))

(defmethod statfs-get ((stat statfs) what)
  "Get a statfs field. 'what' can be any statfs-struct field."
  (foreign-slot-value (ptr stat) 'statfs-struct what))

(defmethod statfs-set ((stat statfs) what new-value)
  "Change to 'new-value' a statfs field. 'what' can be any statfs-struct field."
  (setf (foreign-slot-value (ptr stat) 'statfs-struct what) new-value))

(defsetf statfs-get statfs-set)

(defmethod statfs-copy ((statfs-dest statfs) (statfs-src statfs))
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

