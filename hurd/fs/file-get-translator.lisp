
(in-package :hurd)

(defcfun ("file_get_translator" %file-get-translator)
  err
  (file port)
  (data :pointer)
  (data-len :pointer))

(defconstant +file-get-translator-initial-size+ 1024)

(defun file-get-translator (file)
  "Returns the passive translator set in 'file'."
  (let* ((orig (foreign-alloc :char :count +file-get-translator-initial-size+))
         (buf (foreign-alloc :pointer))
         (len (foreign-alloc 'msg-type-number
                             :initial-element +file-get-translator-initial-size+)))
    (setf (mem-ref buf :pointer) orig)
    (with-cleanup (free-memory-list (list orig buf len))
      (let ((err (%file-get-translator file buf len)))
        (select-error
          err
          (let ((len (mem-ref len 'msg-type-number))
                (ptr (mem-ref buf :pointer)))
            (with-cleanup (unless (pointer-eq ptr orig) (munmap ptr len))
              (foreign-string-zero-separated-to-list ptr len))))))))

