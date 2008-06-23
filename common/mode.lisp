;; filetype
;;
(defconstant +ifmt+ #x0170000)
(defconstant +ifdir+ #x0040000)
(defconstant +ifchr+ #x0020000)
(defconstant +ifblk+ #x0060000)
(defconstant +ifreg+ #x0100000)
(defconstant +iflnk+ #x0120000)
(defconstant +ifsock+ #x0140000)

;; permission bits
;;
; owner
(defconstant +irusr+ #x0400)
(defconstant +iwusr+ #x0200)
(defconstant +ixusr+ #x0100)

; group
(defconstant +irgrp+ (ash +irusr+ -3))
(defconstant +iwgrp+ (ash +iwusr+ -3))
(defconstant +ixgrp+ (ash +ixusr+ -3))

; others
(defconstant +iroth+ (ash +irusr+ -6))
(defconstant +iwoth+ (ash +iwusr+ -6))
(defconstant +ixoth+ (ash +ixusr+ -6))

;; read-only bits
; translators
(defconstant +iptrans+ #x000010000000)
(defconstant +iatrans+ #x000020000000)
(defconstant +iroot+ #x000040000000)

;; isuid
(defconstant +isuid+ #x04000)

;; isgid
(defconstant +isgid+ #x02000)

;; isvtx
(defconstant +isvtx+ #x01000)

;; mmap bits
(defconstant +immap0+ #x000100000000)

;; nocache bits
(defconstant +inocache+ #x000000200000)

;; unknown bits
(defconstant +iuseunk+ #x000000400000)

;(defbitfield mode-flags
;  ; unknown XXX
;  (:iunknown #x000007000000))

(defclass mode ()
  ((value :initform 0
		  :accessor value)))

(defmacro define-mode-meth (name extra-args &body body)
  `(defmethod ,name ((mode mode) ,@(unless (null extra-args) extra-args))
	 (with-accessors ((val value)) mode
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

(defun get-type-bits (type)
  (case type
	(dir +ifdir+)
	(chr +ifchr+)
	(reg +ifreg+)
	(chr +ifchr+)
	(blk +ifblk+)
	(lnk +iflnk+)
	(sock +ifsock+)
	(otherwise
	  +ifmt+))) ; FIXME

(define-mode-meth set-type (new-type)
  (setf (value mode)
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
	(otherwise 0)))

(define-mode-meth has-perms (perm-type user-type)
  (not (zerop (boole boole-and val
					 (get-perm-bits perm-type user-type)))))

(define-mode-meth set-perms (perm-type user-type)
  (setf (value mode)
		(boole boole-ior
			   val
			   (get-perm-bits perm-type user-type)))
  t)

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
     (setf (value mode)
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

(defun create-mode (&key (type 'reg)
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
