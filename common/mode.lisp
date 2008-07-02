;; filetype
;;
(defconstant +ifmt+ #o0170000)
(defconstant +ifdir+ #o0040000)
(defconstant +ifchr+ #o0020000)
(defconstant +ifblk+ #o0060000)
(defconstant +ifreg+ #o0100000)
(defconstant +iflnk+ #o0120000)
(defconstant +ifsock+ #o0140000)

;; permission bits
;;
; owner
(defconstant +irusr+ #o0400)
(defconstant +iwusr+ #o0200)
(defconstant +ixusr+ #o0100)

; group
(defconstant +irgrp+ (ash +irusr+ -3))
(defconstant +iwgrp+ (ash +iwusr+ -3))
(defconstant +ixgrp+ (ash +ixusr+ -3))

; others
(defconstant +iroth+ (ash +irusr+ -6))
(defconstant +iwoth+ (ash +iwusr+ -6))
(defconstant +ixoth+ (ash +ixusr+ -6))

; unknown
(defconstant +irunk+ (ash +irusr+ 12))
(defconstant +iwunk+ (ash +iwusr+ 12))
(defconstant +ixunk+ (ash +ixusr+ 12))

;; read-only bits
; translators
(defconstant +iptrans+ #o000010000000)
(defconstant +iatrans+ #o000020000000)
(defconstant +iroot+ #o000040000000)
(defconstant +itrans+ #o000070000000)

;; isuid
(defconstant +isuid+ #o04000)

;; isgid
(defconstant +isgid+ #o02000)

;; isvtx
(defconstant +isvtx+ #o01000)

;; mmap bits
(defconstant +immap0+ #o000100000000)

;; nocache bits
(defconstant +inocache+ #o000000200000)

;; unknown bits
(defconstant +iuseunk+ #o000000400000)
(defconstant +iunknown+ #o000007000000)

;; spare bits
(defconstant +ispare+ (boole boole-andc2
							 #xffffffff
							 (chained-bit-op boole-ior
									  +ifmt+
									  +itrans+
									  +inocache+
									  +immap0+
									  +iuseunk+
									  +iunknown+
									  #o7777)))

(defgeneric mode-bits (mode))
(defgeneric (setf mode-bits) (val obj))

(defclass base-mode ()
  ())

(defclass mode (base-mode)
  ((mode-bits :initform 0
			  :accessor mode-bits
			  :initarg :mode-bits)))

(defmacro define-mode-meth (name extra-args &body body)
  `(defmethod ,name ((mode base-mode) ,@(unless (null extra-args) extra-args))
	 (with-accessors ((val mode-bits)) mode
	   ,@body)))

(defmacro define-is-type-meth (name bits)
  `(define-mode-meth ,name nil
	 (not (zerop (boole boole-and val ,bits)))))

(define-is-type-meth is-dir-p +ifdir+)
(define-is-type-meth is-chr-p +ifchr+)
(define-is-type-meth is-reg-p +ifreg+)
(define-is-type-meth is-blk-p +ifblk+)
(define-is-type-meth is-lnk-p +iflnk+)
(define-is-type-meth is-sock-p +ifsock+)

(define-mode-meth get-type nil
  (cond
    ((is-dir-p mode) 'dir)
    ((is-chr-p mode) 'chr)
    ((is-reg-p mode) 'reg)
    ((is-blk-p mode) 'blk)
    ((is-lnk-p mode) 'lnk)
    ((is-sock-p mode) 'sock)
    (t
      (warn "Could not get type for mode~%")
      'reg)))

(defun get-type-bits (type)
  (case type
    (dir +ifdir+)
    (reg +ifreg+)
    (chr +ifchr+)
    (blk +ifblk+)
    (lnk +iflnk+)
    (sock +ifsock+)
    (otherwise
      +ifmt+))) ; FIXME

(define-mode-meth set-type (new-type)
  (setf (mode-bits mode)
	(boole boole-ior
	       (boole boole-andc2 val +ifmt+)
	       (get-type-bits new-type)))
  new-type)

;; user-type:
;; owner
;; group
;; others
;;
;; perm-type:
;; read
;; write
;; exec

(defun get-perm-bits (perm-type user-type)
  (case user-type
	(owner
	  (case perm-type
		(read +irusr+)
		(write +iwusr+)
		(exec +ixusr+)
		(otherwise 0)))
	(group
	  (case perm-type
		(read +irgrp+)
		(write +iwgrp+)
		(exec +ixgrp+)
		(otherwise 0)))
	(others
	  (case perm-type
		(read +iroth+)
		(write +iwoth+)
		(exec +ixoth+)
		(otherwise 0)))
	(unknown
	  (case perm-type
		(read +irunk+)
		(write +iwunk+)
		(exec +ixunk+)
		(otherwise 0)))
	(otherwise 0)))

(define-mode-meth has-perms (perm-type user-type)
				  (if (and (eq user-type 'unknown)
	   (not (is-useunk-p mode)))
    nil
    (not (zerop (boole boole-and val
		       (get-perm-bits perm-type user-type))))))

(define-mode-meth set-perms (perm-type user-type)
  (setf (mode-bits mode)
	(boole boole-ior
	       val
	       (get-perm-bits perm-type user-type)))
  t)

(define-mode-meth clear-perms (perm-type user-type)
  (setf (mode-bits mode)
		(boole boole-andc2
		       val
		       (get-perm-bits perm-type user-type)))
  t)

(define-mode-meth set-perms-if (perm-type user-type condit)
  (if condit
	(set-perms mode perm-type user-type)
	(clear-perms mode perm-type user-type)))

(defmacro define-mode-query-meth (name bits)
  `(define-mode-meth ,name nil
	 (not (zerop (boole boole-and val ,bits)))))

(define-mode-query-meth has-passive-trans-p +iptrans+)
(define-mode-query-meth has-active-trans-p +iatrans+)
(define-mode-query-meth is-fs-root-p +iroot+)
(define-mode-query-meth is-uid-p +isuid+)
(define-mode-query-meth is-gid-p +isgid+)
(define-mode-query-meth is-vtx-p +isvtx+)
(define-mode-query-meth is-mmap-p +immap0+)
(define-mode-query-meth is-nocache-p +inocache+)
(define-mode-query-meth is-useunk-p +iuseunk+)

(defmacro define-mode-switcher-meth (name bits)
  `(define-mode-meth ,name (&optional (yes t))
     (setf (mode-bits mode)
		   (if yes
			 (boole boole-ior val ,bits)
			 (boole boole-andc2 val ,bits)))
	 t))

(define-mode-switcher-meth set-uid +isuid+)
(define-mode-switcher-meth set-gid +isgid+)
(define-mode-switcher-meth set-vtx +isvtx+)
(define-mode-switcher-meth set-mmap +immap0+)
(define-mode-switcher-meth set-nocache +inocache+)
(define-mode-switcher-meth set-useunk +iuseunk+)
(define-mode-switcher-meth set-active-trans +iatrans+)
(define-mode-switcher-meth set-passive-trans +iptrans+)
(define-mode-switcher-meth set-trans +itrans+)
(define-mode-switcher-meth set-root +iroot+)
(define-mode-switcher-meth set-types +ifmt+)
(define-mode-switcher-meth set-spare +ispare+)

(defun make-mode-clone (bits)
  (make-instance 'mode :mode-bits bits))

(defun make-mode (&key (type 'reg)
			 (perms '((owner read write) (group read)))
			 (uid nil)
			 (gid nil)
			 (vtx nil)
			 (mmap nil)
			 (nocache nil)
			 (useunk nil))
  (let ((obj (make-instance 'mode)))
    (set-type obj type)
    (mapcar (lambda (owner-list)
	      (let ((owner-type (first owner-list))
		    (perm-list (rest owner-list)))
		(mapcar (lambda (perm-type)
			  (set-perms obj perm-type owner-type))
			owner-list)))
	    perms)
    (set-uid obj uid)
    (set-gid obj gid)
    (set-vtx obj vtx)
    (set-mmap obj mmap)
    (set-nocache obj nocache)
    (set-useunk obj useunk)
    obj))

(defun perm-char (type)
  (case type
    (read #\r)
    (write #\w)
    (write #\x)
    (otherwise #\-)))

(defun type-char (type)
  (case type
    (dir #\d)
    (chr #\c)
    (blk #\b)
    (reg #\-)
    (lnk #\l)
    (sock #\s)
    (otherwise #\-)))

(define-mode-meth print-object (stream)
  (format stream "#<Mode ~c" (type-char (get-type mode)))
  (flet ((show-perm-bits (user-type)
	 (mapcar (lambda (perm-type)
		   (format stream "~c"
			   (cond
			     ((and (eq perm-type 'exec)
				   (eq user-type 'owner)
				   (is-uid-p mode))
			      #\s)
			     ((and (eq perm-type 'exec)
				   (eq user-type 'group)
				   (is-gid-p mode))
			      #\s)
			     ((has-perms mode perm-type user-type)
			      (perm-char perm-type))
			     (t
			       #\-))
			   #\-))
		 '(read write exec))))
    (mapcar #'show-perm-bits '(owner group others)))
  (if (is-vtx-p mode)
    (format stream " vtx"))
  (if (is-mmap-p mode)
    (format stream " mmap"))
  (if (is-nocache-p mode)
    (format stream " nocache"))
  (if (is-useunk-p mode)
    (format stream " useunk"))
  (format stream ">"))

(define-foreign-type mode-type ()
		     ()
		     (:actual-type :unsigned-int)
		     (:simple-parser mode-t))

(defmethod translate-to-foreign (mode (type mode-type))
  (if (null mode)
    0
    (mode-bits mode)))

(defmethod translate-from-foreign (value (type mode-type))
  (make-instance 'mode :mode-bits value))
