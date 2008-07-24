
(in-package :hurd-common)

;;
;; In this file we implement an abstraction to the mode_t type.
;; mode_t is a byte field with information about permissions, types, etc of a node
;;

;;
;; Bits for file types.
;;

(defconstant +ifmt+ #o170000 "Bits for types")
(defconstant +ifdir+ #o040000 "Is a directory")
(defconstant +ifchr+ #o020000 "Is a character device")
(defconstant +ifblk+ #o060000 "Is a block device")
(defconstant +ifreg+ #o0100000 "Regular file")
(defconstant +iflnk+ #o120000 "Link")
(defconstant +ifsock+ #o140000 "Socket")
(defconstant +ififo+ #o10000 "Fifo")

;;
;; Bits for permissions.
;; 4 groups:
;; owner, group, others and unknown.
;; Each one has bits for reading, writing, and exec'ing.
;;

;; owner
(defconstant +irusr+ #o0400 "Read for owner")
(defconstant +iwusr+ #o0200 "Write for owner")
(defconstant +ixusr+ #o0100 "Execute for owner")
(defconstant +iusr+  #o0700 "Bits for owner")

;; group
(defconstant +irgrp+ (ash +irusr+ -3) "Read for group")
(defconstant +iwgrp+ (ash +iwusr+ -3) "Write for group")
(defconstant +ixgrp+ (ash +ixusr+ -3) "Execute for group")
(defconstant +igrp+ (ash +iusr+ -3) "Bits for group")

;; others
(defconstant +iroth+ (ash +irusr+ -6) "Read for others")
(defconstant +iwoth+ (ash +iwusr+ -6) "Write for others")
(defconstant +ixoth+ (ash +ixusr+ -6) "Execute for others")
(defconstant +ioth+ (ash +iusr+ -6) "Bits for others")

;; unknown
(defconstant +irunk+ (ash +irusr+ 12) "Read for unknown")
(defconstant +iwunk+ (ash +iwusr+ 12) "Write for unknown")
(defconstant +ixunk+ (ash +ixusr+ 12) "Execute for unknown")
(defconstant +iunknown+ #o000007000000 "Mask for unknown permissions")

;;
;; These are read-only bits.
;;

;; Translator related bits.
(defconstant +iptrans+ #o000010000000 "Has a passive translator")
(defconstant +iatrans+ #o000020000000 "Has an active translator")
(defconstant +iroot+ #o000040000000 "Is a translator root")
(defconstant +itrans+ #o000070000000 "All the above bits")

(defconstant +immap0+ #o000100000000 "No mmaps to this")

(defconstant +isuid+ #o04000 "Set user ID on execution")

(defconstant +isgid+ #o02000 "Set group ID on execution")

(defconstant +isvtx+ #o01000 "Save swapped text after use (sticky).")

(defconstant +inocache+ #o000000200000 "Don't cache contents for this file")

(defconstant +iuseunk+ #o000000400000 "Use unknown bits")

;; All permission bits.
(defconstant +permission+
  (chained-bit-op boole-ior
                  +iusr+
                  +igrp+
                  +ioth+
                  +iunknown+
                  +iuseunk+))

;; Unused bits.
(defconstant +ispare+ (boole boole-xor
                             #xffffffff
                             (chained-bit-op boole-ior
                                             +ifmt+
                                             +itrans+
                                             +inocache+
                                             +immap0+
                                             +iuseunk+
                                             +iunknown+
                                             +permission+
                                             #o7777)))

;; Define generic functions for accessing and setfing the mode-bits
;; We use them here and in the stat file.

(defgeneric mode-bits (mode))

(defgeneric (setf mode-bits) (val obj))

;; Base classe for mode and stat.
(defclass base-mode () ())

(defclass mode (base-mode)
  ((mode-bits :initform 0
              :accessor mode-bits
              :initarg :mode-bits))
  (:documentation "Mode class for saving a mode_t bitfield"))

(define-foreign-type mode-type ()
  ()
  (:documentation "CFFI mode type.")
  (:actual-type :unsigned-int)
  (:simple-parser mode-t))

(defmethod translate-to-foreign (mode (type mode-type))
  "Translate a mode object to a foreign bit field."
  (if (null mode)
    0
    (mode-bits mode)))

(defmethod translate-from-foreign (value (type mode-type))
  "Translate a foreign bitfield to a mode object."
  (make-instance 'mode :mode-bits value))

(defun %disable-bits (val bits) (boole boole-andc2 val bits))
(defun %only-bits (val bits) (boole boole-and val bits))
(defun %enable-bits (val bits) (boole boole-ior val bits))

(defmacro define-mode-meth (name extra-args doc &body body)
  "Define a new base-mode method with arguments the base-mode object and extra-args.
'val' is accessible, representing the mode bitfield."
  `(defmethod ,name ((mode base-mode) ,@(unless (null extra-args) extra-args))
     ,doc
     (with-accessors ((val mode-bits)) mode
       ,@body)))

(defmacro define-is-type-meth (name bits doc)
  "Defines a new is type method."
  `(define-mode-meth ,name nil
     ,doc
     (eq (%only-bits val +ifmt+) ,bits)))

(define-is-type-meth is-dir-p +ifdir+ "Is a directory?")
(define-is-type-meth is-chr-p +ifchr+ "Is a character device?")
(define-is-type-meth is-reg-p +ifreg+ "Is a regular device?")
(define-is-type-meth is-blk-p +ifblk+ "Is a block device?")
(define-is-type-meth is-lnk-p +iflnk+ "Is a link?")
(define-is-type-meth is-sock-p +ifsock+ "Is a socket?")
(define-is-type-meth is-fifo-p +ififo+ "Is a fifo?")

(define-mode-meth get-type nil
  "Returns type of mode."
  (cond
    ((is-dir-p mode) :dir)
    ((is-chr-p mode) :chr)
    ((is-reg-p mode) :reg)
    ((is-blk-p mode) :blk)
    ((is-lnk-p mode) :lnk)
    ((is-sock-p mode) :sock)
    (t
      (warn "Could not get type for mode~%")
      :unknown)))

(defun %get-type-bits (type)
  "Returns the bits that must be activated from a certain type."
  (case type
    (:dir +ifdir+)
    (:reg +ifreg+)
    (:chr +ifchr+)
    (:blk +ifblk+)
    (:lnk +iflnk+)
    (:sock +ifsock+)
    (:fifo +ififo+)
    (otherwise
      (warn "invalid type at get-type-bits")
      #o000000)))

(define-mode-meth set-type (new-type)
  "Changes type of mode. Possible values for new-type are:
dir, reg, chr, blk, lnk, sock."
  (setf (mode-bits mode)
        (%enable-bits
               (%disable-bits val +ifmt+)
               (%get-type-bits new-type)))
  new-type)

(defun %get-perm-bits (perm-type &optional user-type useunk-p)
  "Returns the permission bytes associated with perm-type and user-type.
These are the possible combinations:

perm-type: read / write / exec
user-type: owner / group / others / unknown

You can also ignore user-type and the bits will be for all the user types.
"
  (if (null user-type)
    (case perm-type
      (read
        (chained-bit-op boole-ior
                        +irusr+
                        +irgrp+
                        +iroth+
                        (if useunk-p +irunk+ 0)))
      (write
        (chained-bit-op boole-ior
                        +iwusr+
                        +iwgrp+
                        +iwoth+
                        (if useunk-p +iwunk+ 0)))
      (exec
        (chained-bit-op boole-ior
                        +ixusr+
                        +ixgrp+
                        +ixoth+
                        (if useunk-p +ixunk+ 0)))
      (otherwise
        0))
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
      (if useunk-p
        (case perm-type
          (read +irunk+)
          (write +iwunk+)
          (exec +ixunk+)
          (otherwise 0))
        0))
    (otherwise 0))))

(define-mode-meth has-perms-p (perm-type user-type)
  "Predicate telling if the mode bitfield has certain permissions. Same combinations as get-perm-bits."
  (let* ((useunk-p (is-useunk-p mode))
        (bits (%get-perm-bits perm-type user-type useunk-p)))
    (and (plusp bits)
         (eq bits
             (%only-bits val bits)))))

(define-mode-meth set-perms (perm-type &optional user-type)
  "Activates permission bits for perm-type/user-type."
  (setf (mode-bits mode)
        (%enable-bits
          val
          (%get-perm-bits perm-type user-type t)))
  t)

(define-mode-meth clear-perms (perm-type &optional user-type)
  "Clears permission bits for perm-type/user-type."
  (setf (mode-bits mode)
        (%disable-bits
               val
               (%get-perm-bits perm-type user-type t)))
  t)

(define-mode-meth set-perms-if (condit perm-type &optional user-type)
  "Activates or clears permission bits based on the 'condit' value."
  (if condit
    (set-perms mode perm-type user-type)
    (clear-perms mode perm-type user-type)))

(defmethod copy-perms ((mode1 base-mode) (mode2 base-mode))
  "Copy all the permission bits from mode1 to mode2."
  (setf (mode-bits mode2)
        (%enable-bits
               (%disable-bits (mode-bits mode2) +permission+)
               (%only-bits (mode-bits mode1) +permission+)))
  mode2)

(defmacro define-mode-query-meth (name bits doc)
  "Defines a new predicate based on 'bits'."
  `(define-mode-meth ,name nil
     ,doc
     (= ,bits (%only-bits val ,bits))))

(define-mode-query-meth has-passive-trans-p +iptrans+ "Has a passive translator?")
(define-mode-query-meth has-active-trans-p +iatrans+ "Has an active translator?")
(define-mode-query-meth is-fs-root-p +iroot+ "Is filesystem root?")
(define-mode-query-meth is-uid-p +isuid+ "Has uid bit?")
(define-mode-query-meth is-gid-p +isgid+ "Has gid bit?")
(define-mode-query-meth is-vtx-p +isvtx+ "Has sticky bit?")
(define-mode-query-meth is-mmap-p +immap0+ "No mmaps on this?")
(define-mode-query-meth is-nocache-p +inocache+ "Don't use caching?")
(define-mode-query-meth is-useunk-p +iuseunk+ "Use unknown permission system?")

(defmacro define-mode-switcher-meth (name bits doc)
  "Creates a new switcher function for 'bits'."
  `(define-mode-meth ,name (&optional (yes t))
     ,doc
     (setf (mode-bits mode)
           (if yes
             (%enable-bits val ,bits)
             (%disable-bits val ,bits)))
     t))

(define-mode-switcher-meth set-uid +isuid+ "Sets uid bit")
(define-mode-switcher-meth set-gid +isgid+ "Sets gid bit")
(define-mode-switcher-meth set-vtx +isvtx+ "Sets sticky bit")
(define-mode-switcher-meth set-mmap +immap0+ "Sets decision on using mmaps")
(define-mode-switcher-meth set-nocache +inocache+ "Sets decision on caching the node")
(define-mode-switcher-meth set-useunk +iuseunk+ "Uses unknown bits")
(define-mode-switcher-meth set-active-trans +iatrans+ "Sets active translator bit")
(define-mode-switcher-meth set-passive-trans +iptrans+ "Sets passive translator bit")
(define-mode-switcher-meth set-trans +itrans+ "Sets all the translator bits")
(define-mode-switcher-meth set-root +iroot+ "Sets root bit")
(define-mode-switcher-meth set-types +ifmt+ "Sets all the type bits")
(define-mode-switcher-meth set-spare +ispare+ "Sets all the spare bits")
(define-mode-switcher-meth set-owner +iusr+ "Set all the owner perm bits")
(define-mode-switcher-meth set-group +igrp+ "Set all the group perm bits")
(define-mode-switcher-meth set-others +ioth+ "Set all the others perm bits")
(define-mode-switcher-meth set-unknown +iunknown+ "Set all the unknown perm bits")

(defun make-mode-clone (bits)
  "Makes a mode object based on 'bits' bitfield."
  (make-instance 'mode :mode-bits bits))

(defun make-mode (&key (type :reg)
                       (perms '((owner read write) (group read))) ; starting permissions
                       (uid nil) ; activate uid bit
                       (gid nil) ; activate gid bit
                       (vtx nil) ; activate sticky bit
                       (mmap nil) ; activate mmap bit
                       (nocache nil) ; activate nocache bit
                       (useunk nil)) ; use unknown bits
  "Creates a new mode object.
'perms' is a list with the form ((user-type1 perm1 perm2 ...) (user-type2 perm1..))."
  (let ((obj (make-instance 'mode)))
    (set-type obj type)
    (mapcar (lambda (owner-list)
              (let ((owner-type (first owner-list))
                    (perm-list (rest owner-list)))
                (mapcar (lambda (perm-type)
                          (set-perms obj perm-type owner-type))
                        perm-list)))
            perms)
    (set-uid obj uid)
    (set-gid obj gid)
    (set-vtx obj vtx)
    (set-mmap obj mmap)
    (set-nocache obj nocache)
    (set-useunk obj useunk)
    obj))

(defun %perm-char (type)
  "Returns the associated character with 'type' permission type."
  (case type
    (read #\r)
    (write #\w)
    (exec #\x)
    (otherwise #\-)))

(defun %type-char (type)
  "Returns the associated character with 'type' file type."
  (case type
    (:dir #\d)
    (:chr #\c)
    (:blk #\b)
    (:reg #\-)
    (:lnk #\l)
    (:sock #\s)
    (otherwise #\-)))

(define-mode-meth print-object (stream)
  "Prints a mode object."
  (format stream "#<Mode ~c" (%type-char (get-type mode)))
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
                                             ((has-perms-p mode perm-type user-type)
                                              (%perm-char perm-type))
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
