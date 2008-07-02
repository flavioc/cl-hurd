
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

(defclass open-flags ()
  ((value-bits :documentation "General it field for flags used for: file access, translation, IO operating modes, syynchronization, blocking, etc"
	  :accessor value-bits
	  :initarg :value-bits)))

(defmacro define-flags-meth (name extra-args &body body)
  `(defmethod ,name ((flags open-flags)
		     ,@(unless (null extra-args) extra-args))
     (with-accessors ((val value-bits)) flags
       ,@body)))

(define-flags-meth is (flag)
  (not (zerop
	 (boole boole-and
		(flag-to-bits flag)
		val))))

(defun make-flag (ls)
  (make-instance 'open-flags
		 :value-bits (list-to-flags ls)))

(defun disable-flags (val flags)
  (boole boole-andc2
	 val
	 (list-to-flags flags)))

(defun enable-flags (val flags)
  (boole boole-ior
	 val
	 (list-to-flags flags)))

(define-flags-meth enable (new-flags)
  (setf (value-bits flags)
	(enable-flags val new-flags))
  new-flags)

(define-flags-meth enabled (new-flags)
  (make-instance 'open-flags
		 :value-bits (enable-flags val new-flags)))

(define-flags-meth disabled (old-flags)
  (make-instance 'open-flags
		 :value-bits (disable-flags val old-flags)))

(define-flags-meth disable (old-flags)
 (setf (value-bits flags)
       (disable-flags val old-flags))
 old-flags)

(define-flags-meth only (only-flags)
  (setf (value-bits flags)
		(boole boole-and val (list-to-flags only-flags))))

(defconstant +flags-list+
	     (list
	       (list 'read +o-read+)
	       (list 'write +o-write+)
	       (list 'exec +o-exec+)
	       (list 'norw +o-norw+)
	       (list 'largefile +o-largefile+)
	       (list 'creat +o-creat+)
	       (list 'excl +o-excl+)
	       (list 'nolink +o-nolink+)
	       (list 'notrans +o-notrans+)
	       (list 'nofollow +o-nofollow+)
	       (list 'directory +o-directory+)
	       (list 'append +o-append+)
	       (list 'async +o-async+)
	       (list 'fsync +o-fsync+)
	       (list 'sync +o-sync+)
	       (list 'noatime +o-noatime+)
	       (list 'shlock +o-shlock+)
	       (list 'exlock +o-exlock+)
	       (list 'dsync +o-dsync+)
	       (list 'rsync +o-rsync+)
	       (list 'nonblock +o-nonblock+)
	       (list 'hurd +o-hurd+)
	       (list 'trunc +o-trunc+)
	       (list 'cloexec +o-cloexec+)
		   (list 'non-open-modes (chained-bit-op boole-ior
												 +o-creat+
												 +o-excl+
												 +o-nolink+
												 +o-notrans+))))

(defun flag-to-bits (flag)
  (let ((result (find flag +flags-list+ :key #'first)))
    (cond
      (result
	(second result))
      (t
	(warn "Flag ~s not recognized" flag)
	0))))

(defun list-to-flags (ls)
  (cond
	((null ls)
	 0)
	((symbolp ls)
	 (flag-to-bits ls))
	(t
	  (boole boole-ior
			 (flag-to-bits (first ls))
			 (list-to-flags (rest ls))))))

(define-flags-meth print-object (stream)
  (format stream "#<open-flags")
  (mapcar (lambda (flag)
	    (if (is flags flag)
	      (format stream " ~s" flag)))
	  (mapcar #'first +flags-list+))
  (format stream ">"))

(define-foreign-type open-flags-type ()
		     ()
		     (:actual-type :int)
		     (:simple-parser open-flags-t))

(defmethod translate-to-foreign (flags (type open-flags-type))
  (if (or (null flags)
	  (listp flags))
    (value-bits (make-flag flags))
    (value-bits flags)))

(defmethod translate-from-foreign (value (type open-flags-type))
  (make-instance 'open-flags :value-bits value))
