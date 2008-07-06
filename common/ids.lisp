
(in-package :hurd-common)

(defctype uid-t :int "Represents an user ID")

(defctype gid-t :int "Represents a group ID")

(defun valid-id-p (id)
  "Checks if the ID (uid or gid) is valid."
  (and (numberp id)
       (>= id 0)))
