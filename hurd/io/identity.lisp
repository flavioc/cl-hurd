
(in-package :hurd)

(defcfun ("io_identity" %io-identity)
  err
  (file port)
  (idport port-pointer)
  (fsidport port-pointer)
  (fileno :pointer))

(defun io-identity (file)
  "Return the identity port the filesystem id port and the fileno."
  (declare (type fixnum file))
  (with-foreign-pointer (idport (foreign-type-size 'port))
    (with-foreign-pointer (fsidport (foreign-type-size 'port))
      (with-foreign-pointer (fileno (foreign-type-size 'ino-t))
        (select-error (%io-identity file idport fsidport fileno)
                      (values
                        (mem-ref idport 'port)
                        (mem-ref fsidport 'port)
                        (mem-ref fileno 'ino-t)))))))

