
(in-package :hurd-common)

;;
;; This file implements the 'open-flags-t' CFFI type.
;; It is associated with the various flags used in file
;; opening operations, like dir_lookup (and, of course, file_name_lookup), etc.
;;

;; 
;; These are the possible flags and associated bits
;; that are declared at bits/fcntl.h.
;; See that file for more information about each one.
;;

(defconstant +o-norw+ #x0000)
(defconstant +o-read+ #x0001)
(defconstant +o-write+ #x0002)
(defconstant +o-exec+ #x0004)
(defconstant +o-largefile+ 0)
(defconstant +o-creat+ #x0010)
(defconstant +o-excl+ #x0020)
(defconstant +o-nolink+ #x0040)
(defconstant +o-notrans+ #x0080)
(defconstant +o-nofollow+ #x00100000)
(defconstant +o-directory+ #x00200000)
(defconstant +o-append+ #x0100)
(defconstant +o-async+ #x0200)
(defconstant +o-fsync+ #x0400)
(defconstant +o-sync+ +o-fsync+)
(defconstant +o-noatime+ #x0800)
(defconstant +o-shlock+ #x00020000)
(defconstant +o-exlock+ #x00040000)
(defconstant +o-dsync+ +o-sync+)
(defconstant +o-rsync+ +o-sync+)
(defconstant +o-nonblock+ #x0008)

(defconstant +o-hurd+ (boole boole-ior
                             #xffff
                             (boole boole-ior
                                    +o-exlock+
                                    +o-shlock+)))

(defconstant +o-trunc+ #x00010000)
(defconstant +o-cloexec+ #x00400000)

;;
;; We group all the flags in a list to use
;; in the translate-*-foreign functions.
;;
(defconstant +flags-list+
  `((:read ,+o-read+)
    (:write ,+o-write+)
    (:exec ,+o-exec+)
    (:norw ,+o-norw+)
    (:largefile ,+o-largefile+)
    (:creat ,+o-creat+)
    (:excl ,+o-excl+)
    (:nolink ,+o-nolink+)
    (:notrans ,+o-notrans+)
    (:nofollow ,+o-nofollow+)
    (:directory ,+o-directory+)
    (:append ,+o-append+)
    (:async ,+o-async+)
    (:fsync ,+o-fsync+)
    (:sync ,+o-sync+)
    (:noatime ,+o-noatime+)
    (:shlock ,+o-shlock+)
    (:exlock ,+o-exlock+)
    (:dsync ,+o-dsync+)
    (:rsync ,+o-rsync+)
    (:nonblock ,+o-nonblock+)
    (:hurd ,+o-hurd+)
    (:trunc ,+o-trunc+)
    (:cloexec ,+o-cloexec+)
    (:open-modes ,(chained-bit-op boole-ior
                                  +o-creat+
                                  +o-excl+
                                  +o-nolink+
                                  +o-notrans+))))

(defun %flag-to-bits (flag)
  "Returns the bits associated with 'flag'."
  (let ((result (find flag +flags-list+ :key #'first)))
    (cond
      (result
        (second result))
      (t
        ; Warn the user about it
        ; but return 0
        (warn "Flag ~s not recognized" flag)
        0))))

(defun %get-flag-bits (obj)
  "Returns the 'obj' flags in the bitfield representation."
  (if (integerp obj)
    obj ; Simple bitfield value
    (case (type-of obj)
      ; This is nil. Just return 0.
      (null 0)
      ; This a keyword, which means is only one flag, return its representation.
      (keyword (%flag-to-bits obj))
      ; This is a list of flags, return all the bits of each flag or-ed.
      (cons (boole boole-ior
                   (%flag-to-bits (first obj))
                   (%get-flag-bits (rest obj))))
      ; This is an open-flags object, return the internal bitfield.
      (open-flags (value-bits obj))
      (otherwise
        (warn "Unrecognized object type ~s in %get-flag-bits" (type-of obj))
        0))))

(defclass open-flags ()
  ((value-bits
     :documentation "General bitfield for flags used for: file access, translation, IO operating modes, synchronization, blocking, etc"
     :accessor value-bits
     :initarg :value-bits))
  (:documentation
    "This class implements the Lisp counterpart of the CFFI 'open-flags-t' type. Allocates the flags as a bitfield."))

(defmacro define-flags-meth (name extra-args doc &body body)
  "Defines a new flags method, name is the method name and extra-args are the arguments used with always present flags argument. In body, 'val' is bound to the original flag bits." 
  `(defmethod ,name ((flags open-flags)
                     ,@(unless (null extra-args) extra-args))
     ,doc
     (with-accessors ((val value-bits)) flags
       ,@body)))

(define-flags-meth flag-is-p (flag)
  "Checks if our flag bitfield has a certain flag activated."
  (let ((flag-bits (%flag-to-bits flag)))
    (eq flag-bits
        (boole boole-and flag-bits val))))

(defun disable-flags (val flags)
  "Returns a new bitfield disabling from 'val' the flags in 'flags'."
  (boole boole-andc2
         val
         (%get-flag-bits flags)))

(defun enable-flags (val flags)
  "Returns a new bitfield enabling from 'val' the flags in 'flags'."
  (boole boole-ior
         val
         (%get-flag-bits flags)))

(define-flags-meth enable (new-flags)
  "Destructively enable flags 'new-flags' in this bitfield."
  (setf (value-bits flags)
        (enable-flags val new-flags))
  new-flags)

(define-flags-meth enabled (new-flags)
  "Returns a new bitfield enabling flags 'new-flags'."
  (make-flag (enable-flags val new-flags)))

(define-flags-meth disable (old-flags)
 "Destructively disable flags 'old-flags' in this bitfield."
 (setf (value-bits flags)
       (disable-flags val old-flags))
 old-flags)

(define-flags-meth disabled (old-flags)
  "Returns a new bitfield disabling flags 'old-flags'."
  (make-flag (disable-flags val old-flags)))

(define-flags-meth only (only-flags)
  "Destructively disable all the other flags except the ones in 'only-flags'."
  (setf (value-bits flags)
        (boole boole-and val (%get-flag-bits only-flags))))

(define-flags-meth print-object (stream)
  "Prints the open-flag object to stream."
  (format stream "#<open-flags")
  ; Check if each flag is present and print it!
  (mapcar (lambda (flag)
            (if (flag-is-p flags flag)
              (format stream " ~s" flag)))
          (mapcar #'first +flags-list+))
  (format stream ">"))

(defun make-flag (obj)
  "Make an instance of open-flags"
  (make-instance 'open-flags
                 :value-bits (%get-flag-bits obj)))

(define-foreign-type open-flags-type ()
  ()
  (:documentation "Represents the CFFI open-flags-t type.")
  (:actual-type :int)
  (:simple-parser open-flags-t))

(defmethod translate-to-foreign (flags (type open-flags-type))
  "Translates a Lisp flag object (which can be a symbol, an open-flags object, a list, an integer value, etc) to a foreign value."
  (%get-flag-bits flags))

(defmethod translate-from-foreign (value (type open-flags-type))
  "Translates a foreign bitfield to an open-flags object."
  (make-flag value))
