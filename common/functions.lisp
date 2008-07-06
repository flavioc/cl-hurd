
(in-package :hurd-common)

;;
;; This files declares some foreign functions that
;; still don't deserve a single file for each one.
;;

(defcfun ("strerror" %strerror) :string (code err))

(defun error->string (error-code)
  "Translates an error code to a string."
  (%strerror error-code))

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
