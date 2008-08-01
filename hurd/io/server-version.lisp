
(in-package :hurd)

(defcfun ("io_server_version" %io-server-version)
  err
  (object port)
  (server-name :string)
  (server-major-version :pointer)
  (server-minor-version :pointer)
  (server-edit-level :pointer))

(defun io-server-version (port)
  "Return a list with name and 3 versions of a filesystem port."
  (with-foreign-object (server-name :char 1024) ; as in hurd/hurd_types.defs (string_t)
	  (with-foreign-object (version :int 3)
      ; Initialize version array with zeroes.
      (dotimes (i 3)
        (setf (mem-aref version :int i) 0))
      (setf (mem-aref server-name :char 0) 0)
      (let ((error-code (%io-server-version
                          port
                          server-name
                          version
                          (inc-pointer version (foreign-type-size :int))
                          (inc-pointer version (* 2 (foreign-type-size :int))))))
        (select-error
          error-code
          (append (list (foreign-string-to-lisp server-name))
                  (loop for i from 0 below 3
                        collect (mem-aref version :int i))))))))
