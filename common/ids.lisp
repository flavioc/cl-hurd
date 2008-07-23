
(in-package :hurd-common)

(defctype uid-t :int "Represents an user ID")

(defctype gid-t :int "Represents a group ID")

(defun valid-id-p (id)
  "Checks if the ID (uid or gid) is valid."
  (and (numberp id)
       (>= id 0)))

(defconstant +uid-t-size+ (foreign-type-size 'uid-t))
(defconstant +gid-t-size+ (foreign-type-size 'gid-t))

(defcfun ("getuid" %getuid) :unsigned-int)

(defun getuid ()
  "Get the real user ID of the process."
  (%getuid))

(defcfun ("geteuid" %geteuid) :unsigned-int)

(defun geteuid ()
  "Get the effective user ID of the process."
  (%geteuid))

(defcfun ("getgid" %getgid) :unsigned-int)

(defun getgid ()
  "Get the real group ID of the process."
  (%getgid))

(defcfun ("getegid" %getegid) :unsigned-int)

(defun getegid ()
  "Get the effective group ID of the process."
  (%getegid))
