
(in-package :hurd-common)

;;
;; This files declares some foreign functions that
;; still don't deserve a single file for each one.
;;

(defcfun ("strerror" %strerror) :string (code err))

(defun error->string (error-code)
  "Translates an error code to a string."
  (%strerror error-code))

(defcfun ("getpid" %getpid) pid-t)

(defun getpid ()
  (%getpid))
