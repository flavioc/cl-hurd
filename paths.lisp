
(in-package :cl-user)

(let ((hurd-pathname (asdf:component-pathname
                       (asdf:find-system 'hurd))))
  (push (merge-pathnames #p"hurd/helper-libs/"
                         hurd-pathname)
        cffi:*foreign-library-directories*)
  (push (merge-pathnames #p"stubs/"
                         hurd-pathname)
        cffi:*foreign-library-directories*))

