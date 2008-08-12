
(in-package :hurd-translator)

(defconstant +open-create-flags+
  '(:creat :excl :nolink :notrans)
  "Flags to pass when creating a new file.")

