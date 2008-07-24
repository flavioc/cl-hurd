
(in-package :hurd)

(defun %concat-downcase (path str)
  "Concatenates path and str and then returns that downcased."
  (string-downcase (concatenate-string path str)))

(defconstant +servers+ "/servers/" "Servers path.")

(defmacro %define-server-path (name str)
  "Defines a new server path."
  `(defconstant ,name
      (%concat-downcase +servers+ ,str)))

(%define-server-path +servers-crash+ "crash")
(%define-server-path +servers-exec+ "exec")
(%define-server-path +servers-proc+ "proc")
(%define-server-path +servers-password "password")
(%define-server-path +servers-socket "socket")

(defconstant +hurd+ "/hurd/" "Hurd path.")

(defmacro %define-hurd-path (name str)
  "Defines a new hurd path."
  `(defconstant ,name
		 (%concat-downcase +hurd+ ,str)))

(%define-hurd-path +hurd-init+ "init")
(%define-hurd-path +hurd-proc+ "proc")
(%define-hurd-path +hurd-auth+ "auth")

(%define-hurd-path +hurd-symlink+ "symlink")
(%define-hurd-path +hurd-chrdev+ "chrdev")
(%define-hurd-path +hurd-blkdev+ "blkdev")
(%define-hurd-path +hurd-fifo+ "fifo")
(%define-hurd-path +hurd-ifsock+ "ifsock")

