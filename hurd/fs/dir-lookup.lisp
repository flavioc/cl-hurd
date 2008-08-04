
(in-package :hurd)

(defcfun ("dir_lookup" %dir-lookup)
  err
  (dir port)
  (file-name :string)
  (flags open-flags)
  (mode mode-t)
  (do-retry :pointer)
  (retry-name :pointer)
  (result port-pointer))

(defun dir-lookup (dir file-name &key (flags '()) (mode (make-mode)))
  (declare (type fixnum dir)
           (type string file-name)
           (type list flags)
           (type mode mode))
  (with-foreign-pointer (do-retry (foreign-type-size 'retry-type))
    (with-foreign-pointer (retry-name 1024) ; Same as libc's hurdlookup.c
      (with-foreign-pointer (result (foreign-type-size 'port))
        (let ((err (%dir-lookup dir file-name flags mode
                                do-retry retry-name result)))
          (select-error err
                        (values
                          (mem-ref do-retry 'retry-type)
                          (let ((val (foreign-string-to-lisp retry-name)))
                            (cond
                              ((null val) "")
                              (t val)))
                          (mem-ref result 'port))))))))

